use super::audio::{AudioBackend, SoundController};
use super::cpu::Cpu;
use super::mem::{Cartridge, GBMemory};
use super::ppu::{FrameBuffer, Ppu};
use crate::util::{Error, GBEResult};

use std::cell::RefCell;
use std::fs::read;
use std::path::Path;
use std::rc::Rc;
use std::sync::{mpsc, Arc, Mutex};

pub struct Package {
    sound_controller: Rc<RefCell<SoundController>>,
    cpu: Rc<RefCell<Cpu<GBMemory>>>,
    ppu: Ppu<GBMemory, Cpu<GBMemory>>,
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
    ) -> GBEResult<Self> {
        let sound_controller = Rc::new(RefCell::new(SoundController::new(audio_backend)));

        let cartridge_bytes = read(rom)?;
        let cartridge = Cartridge::new(&cartridge_bytes);
        let memory = if let Some(bootrom_path) = bootrom {
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
        let cpu = Rc::new(RefCell::new(Cpu::new(memory.clone())));
        let ppu = Ppu::with_external_fb(memory.clone(), cpu.clone(), fb);
        Ok(Package {
            sound_controller,
            cpu,
            ppu,
        })
    }

    pub fn run(&mut self, time_passed: mpsc::Receiver<i64>, control: Arc<Mutex<EmulatorControl>>) {
        let mut time = time_passed.recv().unwrap();
        let mut next_time = time;
        'main: loop {
            let frame;
            if *control.lock().unwrap() == EmulatorControl::Run {
                frame = self.ppu.tick();
                self.sound_controller.borrow_mut().tick();
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
