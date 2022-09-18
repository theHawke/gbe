use super::cpu::{InterruptType, Interruptible};
use super::mem::MemoryIfc;

use std::cell::{RefCell, RefMut};
use std::rc::Rc;

pub struct Timer<M, I> {
    tick_count: u64,
    mem: Rc<RefCell<M>>,
    cpu: Rc<RefCell<I>>,
}

impl<M: MemoryIfc, I: Interruptible> Timer<M, I> {
    pub fn new(mem: Rc<RefCell<M>>, cpu: Rc<RefCell<I>>) -> Self {
        Timer {
            tick_count: 0,
            mem,
            cpu,
        }
    }

    pub fn tick(&mut self) {
        self.tick_count += 1;
        let mut mem_cr = RefMut::map(self.mem.borrow_mut(), |m| m.get_cr_mut());
        if self.tick_count & 0xFF == 0 {
            mem_cr.timer_div = mem_cr.timer_div.wrapping_add(1);
        }
        let ctrl = mem_cr.timer_ctrl;
        if ctrl & 0x04 != 0 {
            let mask = match ctrl & 0x03 {
                0x00 => 0x3FF,
                0x01 => 0x00F,
                0x02 => 0x03F,
                0x03 => 0x0FF,
                _ => panic!(),
            };
            if self.tick_count & mask == 0 {
                let (new_val, of) = mem_cr.timer_ctr.overflowing_add(1);
                mem_cr.timer_ctr = if of {
                    mem_cr.timer_mod
                } else {
                    new_val
                };
                if of {
                    std::mem::drop(mem_cr);
                    self.cpu.borrow_mut().raise_interrupt(InterruptType::Timer);
                }
            }
        }
    }
}
