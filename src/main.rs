mod emucore;

use std::rc::Rc;
use std::cell::RefCell;

fn main() {
    println!("Hello, world!");
    let cartridge = Box::new(emucore::mem::Cartridge::new(&[0]));
    let mem = Rc::new(RefCell::new(emucore::mem::MemoryController::new(cartridge)));
    let mut cpu = emucore::cpu::Cpu::new(mem);
    cpu.tick();
}