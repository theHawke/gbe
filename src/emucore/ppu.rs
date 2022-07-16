use super::mem::MemoryIfc;
use super::cpu::{Interruptible, InterruptType};



pub struct Ppu<M: MemoryIfc, C: Interruptible> {
    mem: Rc<RefCell<M>>,
    cpu: Rc<RefCell<C>>,
}