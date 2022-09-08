use super::audio::{AudioBackend, SoundController};
use super::cpu::Cpu;
use super::mem::{Cartridge, GBMemory};
use super::ppu::{FrameBuffer, Ppu};
use super::timer::Timer;
use super::joypad::JoyPad;
use crate::util::{Error, GBEResult};

use std::cell::RefCell;
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
pub enum EmulatorControl {
    Run,
    Pause,
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

    pub fn run(&mut self, time_passed: mpsc::Receiver<i64>, control: Arc<Mutex<EmulatorControl>>) {
        let mut time = time_passed.recv().unwrap();
        let mut next_time = time;
        'main: loop {
            let frame;
            if *control.lock().unwrap() == EmulatorControl::Run {
                self.sound_controller.borrow_mut().tick();
                frame = self.ppu.tick();
                self.timer.tick();
                self.joypad.tick();
                self.cpu.borrow_mut().tick();
            } else {
                frame = false;
            }
            if frame {
                // notify main thread to redraw

                // wait for notification to run next frame
                while next_time < time {
                    next_time = match time_passed.recv() {
                        Ok(t) => t,
                        Err(_) => {
                            eprintln!("timing sender shut down => stopping emulator");
                            break 'main;
                        }
                    }
                }
                time += 16742; // (59.73 Hz)^-1 in µs
            }
        }
    }
}
