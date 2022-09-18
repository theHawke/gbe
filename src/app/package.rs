use super::timing::TimingReceiver;
use crate::emucore::{
    audio::{AudioBackend, SoundController},
    cpu::Cpu,mem::{Cartridge, GBMemory},
    ppu::{FrameBuffer, Ppu},
    timer::Timer,
    joypad::JoyPad
};
use crate::util::{Error, GBEResult};

use std::{cell::RefCell, sync::mpsc::TryRecvError};
use std::fs::read;
use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::AtomicU8;
use std::sync::{mpsc, Arc, Mutex};

pub struct Package {
    sound_controller: Rc<RefCell<SoundController>>,
    cpu: Rc<RefCell<Cpu<GBMemory>>>,
    ppu: Ppu<GBMemory, Cpu<GBMemory>>,
    timer: Timer<GBMemory, Cpu<GBMemory>>,
    joypad: JoyPad<GBMemory, Cpu<GBMemory>>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Command {
    Run {
        from_time: i64,
    },
    Pause,
    Stop,
    DebugFrame,
    DebugStep,
    DebugCycle,
}

fn load_bootrom_bytes(path: &Path) -> GBEResult<Box<[u8; 256]>> {
    let bytes = read(path)?;
    if bytes.len() != 256 {
        Err(Error::BootromSizeError(bytes.len()))
    } else {
        let mut ret = Box::new([0; 256]);
        ret.copy_from_slice(&bytes);
        Ok(ret)
    }
}

impl Package {
    pub fn new(
        rom: &Path,
        bootrom: Option<&Path>,
        fb: Arc<Mutex<FrameBuffer>>,
        audio_backend: Box<dyn AudioBackend>,
        buttons: Arc<AtomicU8>,
    ) -> GBEResult<Self> {
        let sound_controller = Rc::new(RefCell::new(SoundController::new(audio_backend)));

        let cartridge_bytes = read(rom)?;
        let cartridge = Cartridge::new(&cartridge_bytes);
        let mem = if let Some(bootrom_path) = bootrom {
            let bootrom_bytes = load_bootrom_bytes(bootrom_path)?;
            Rc::new(RefCell::new(GBMemory::with_bootrom(
                bootrom_bytes,
                cartridge,
                sound_controller.clone(),
            )))
        } else {
            Rc::new(RefCell::new(GBMemory::new(
                cartridge,
                sound_controller.clone(),
            )))
        };
        let cpu = Rc::new(RefCell::new(Cpu::new(mem.clone())));
        let ppu = Ppu::with_external_fb(mem.clone(), cpu.clone(), fb);
        let timer = Timer::new(mem.clone(), cpu.clone());
        let joypad = JoyPad::new(mem.clone(), cpu.clone(), buttons);
        Ok(Package {
            sound_controller,
            cpu,
            ppu,
            timer,
            joypad,
        })
    }

    fn tick(&mut self) {
        self.sound_controller.borrow_mut().tick();
        self.ppu.tick();
        self.timer.tick();
        self.joypad.tick();
        self.cpu.borrow_mut().tick();
    }

    fn run_instr(&mut self) {
        loop {
            self.tick();
            if self.cpu.borrow().new_instr() {
                break;
            }
        }
    }

    fn run_frame(&mut self, debug: bool) {
        loop {
            self.tick();
            if debug && self.cpu.borrow().new_instr() {
                // check debug breakpoints
            }
            if self.ppu.frame_start() {
                break;
            }
        }
    }

    pub fn run(&mut self, timing: TimingReceiver, commands: mpsc::Receiver<Command>) {
        let mut running = false;
        let mut next_frame = 0;
        loop {
            match commands.try_recv() {
                Ok(Command::Run{from_time}) => {
                    next_frame = from_time;
                    running = true;
                }
                Ok(Command::Pause) => {
                    running = false;
                }
                Ok(Command::Stop) => {
                    println!("Stopping emulator.");
                    break;
                }
                Ok(Command::DebugCycle) => {
                    running = false;
                    self.tick();
                }
                Ok(Command::DebugStep) => {
                    running = false;
                    self.run_instr();
                }
                Ok(Command::DebugFrame) => {
                    running = false;
                    self.run_frame(true);
                }
                Err(TryRecvError::Disconnected) => {
                    eprintln!("Command queue disconnected. Shutting down emulator.");
                    break;
                }
                Err(TryRecvError::Empty) => {}
            }

            if running {
                loop {
                    timing.wait();
                    if timing.get() > next_frame {
                        break;
                    }
                }
                self.run_frame(false);
                next_frame += 16742; // (59.73 Hz)^-1 in µs
            }
            else {
                timing.wait();
            }
        }
    }
}
