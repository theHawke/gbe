mod emucore;
mod app;

use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    println!("Hello, world!");
    let cartridge = emucore::mem::Cartridge::new(&[0]);
    let backend = Box::new(app::audio_backend::CpalBackend::new());
    let audio = Rc::new(RefCell::new(emucore::audio::SoundController::new(backend)));
    let mem = Rc::new(RefCell::new(emucore::mem::GBMemory::new(
        cartridge,
        audio.clone(),
    )));
    let mut cpu = emucore::cpu::Cpu::new(mem);
    cpu.tick();
    audio.borrow_mut().tick();
}
