use super::cpu::{InterruptType, Interruptible};
use super::mem::MemoryIfc;

use std::cell::{RefCell, RefMut};
use std::rc::Rc;

use std::sync::{
    atomic::{AtomicU8, Ordering},
    Arc,
};

pub struct JoyPad<M, I> {
    tick_count: u64,
    mem: Rc<RefCell<M>>,
    cpu: Rc<RefCell<I>>,
    buttons: Arc<AtomicU8>,
}

fn bool_to_mask(b: bool) -> u8 {
    (-(b as i8)) as u8
}

const JOYPAD_POLL_MASK: u64 = 0x03;

impl<M: MemoryIfc, I: Interruptible> JoyPad<M, I> {
    pub fn new(mem: Rc<RefCell<M>>, cpu: Rc<RefCell<I>>, buttons: Arc<AtomicU8>) -> Self {
        JoyPad {
            tick_count: 0,
            mem,
            cpu,
            buttons,
        }
    }

    pub fn tick(&mut self) {
        self.tick_count += 1;
        if self.tick_count & JOYPAD_POLL_MASK == 0 {
            let mut mem_cr = RefMut::map(self.mem.borrow_mut(), |m| m.get_cr_mut());
            let mut reg = mem_cr.joypad_read;
            if let Some(new) = mem_cr.joypad_write {
                reg = new & 0xF0 | reg & 0x0F;
                mem_cr.joypad_write = None;
            }
            let direction = bool_to_mask(reg & 0x10 == 0); // active low
            let action = bool_to_mask(reg & 0x20 == 0); // active low
            let buttons = self.buttons.load(Ordering::Relaxed);
            let new = (direction | buttons) & ((action | buttons) >> 4) & 0x0F;
            if 0x0F & reg & !new != 0 {
                // raise interrupt if any of the bits 0-3 in the register transition HIGH -> LOW
                self.cpu.borrow_mut().raise_interrupt(InterruptType::JoyPad);
            }
            reg = new & 0x0F | reg & 0xF0;
            mem_cr.joypad_read = reg;
        }
    }
}
