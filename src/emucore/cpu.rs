use super::mem::MemoryIfc;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Cpu<M: MemoryIfc> {
    state: CpuState,
    mem: Rc<RefCell<M>>,
    opcode: Opcode,
    dispatch: InstructionFn<M>,
    mcycle: MCycle,
    temp8: u8,
    temp16: u16,
}

#[derive(Debug, PartialEq)]
struct CpuState {
    state: State,
    reg: RegisterFile,
    ime: bool, // interrupt master enable
}

impl CpuState {
    fn new() -> CpuState {
        CpuState {
            state: State::Running,
            reg: RegisterFile::new(),
            ime: true,
        }
    }
    fn display(&self) {
        println!("State: {:?}", self.state);
        println!("Registers:");
        let b = self.reg.b();
        let c = self.reg.c();
        println!("B: {:x} C: {:x} BC: {:x}", b, c, self.reg.bc);
        let d = self.reg.d();
        let e = self.reg.e();
        println!("D: {:x} E: {:x} DE: {:x}", d, e, self.reg.de);
        let h = self.reg.h();
        let l = self.reg.l();
        println!("H: {:x} L: {:x} HL: {:x}", h, l, self.reg.hl);
        let a = self.reg.a();
        let f = self.reg.f();
        println!("A: {:x} F: {:x} AF: {:x}", a, f, self.reg.af);
        println!("SP: {:x}", self.reg.sp);
        println!("PC: {:x}", self.reg.pc);
        println!("IME: {}", self.ime);
        println!("");
    }
}

#[derive(Debug, PartialEq)]
enum State {
    Running,
    Stop,
    Halt,
    Error,
}

#[derive(Debug, PartialEq)]
struct RegisterFile {
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    af: u16,
    pc: u16,
}

impl RegisterFile {
    fn new() -> RegisterFile {
        RegisterFile {
            bc: 0,
            de: 0,
            hl: 0,
            sp: 0,
            af: 0,
            pc: 0,
        }
    }

    fn b(&self) -> u8 {
        (self.bc >> 8) as u8
    }
    fn set_b(&mut self, val: u8) {
        self.bc &= 0x00FF;
        self.bc |= (val as u16) << 8;
    }
    fn c(&self) -> u8 {
        self.bc as u8
    }
    fn set_c(&mut self, val: u8) {
        self.bc &= 0xFF00;
        self.bc |= val as u16;
    }
    fn d(&self) -> u8 {
        (self.de >> 8) as u8
    }
    fn set_d(&mut self, val: u8) {
        self.de &= 0x00FF;
        self.de |= (val as u16) << 8;
    }
    fn e(&self) -> u8 {
        self.de as u8
    }
    fn set_e(&mut self, val: u8) {
        self.de &= 0xFF00;
        self.de |= val as u16;
    }
    fn h(&self) -> u8 {
        (self.hl >> 8) as u8
    }
    fn set_h(&mut self, val: u8) {
        self.hl &= 0x00FF;
        self.hl |= (val as u16) << 8;
    }
    fn l(&self) -> u8 {
        self.hl as u8
    }
    fn set_l(&mut self, val: u8) {
        self.hl &= 0xFF00;
        self.hl |= val as u16;
    }
    fn a(&self) -> u8 {
        (self.af >> 8) as u8
    }
    fn set_a(&mut self, val: u8) {
        self.af &= 0x00F0;
        self.af |= (val as u16) << 8;
    }
    fn f(&self) -> u8 {
        self.af as u8 & 0x0F
    }
    fn set_f(&mut self, val: u8) {
        self.af &= 0xFF00;
        self.af |= (val & 0xF0) as u16;
    }
    fn update_flags(&mut self, set: u8, reset: u8, new: u8, mask: u8) {
        // mask selects values from new to be applied
        self.af &= !(mask as u16);
        self.af |= (new & mask) as u16;
        self.af &= !(reset as u16);
        self.af |= set as u16;
    }
    fn zf(&self) -> bool {
        (self.af & 0x80) != 0
    }
    fn nf(&self) -> bool {
        (self.af & 0x40) != 0
    }
    fn hf(&self) -> bool {
        (self.af & 0x20) != 0
    }
    fn cf(&self) -> bool {
        (self.af & 0x10) != 0
    }
    fn r8_read(&self, idx: u8) -> u8 {
        // idx == 6 is invalid, usually used by (HL) memory indirection
        match idx {
            0 => self.b(),
            1 => self.c(),
            2 => self.d(),
            3 => self.e(),
            4 => self.h(),
            5 => self.l(),
            7 => self.a(),
            _ => panic!("r8_read only valid for idx 0-5,7"),
        }
    }
    fn r8_write(&mut self, idx: u8, val: u8) {
        // idx == 6 is invalid, usually used by (HL) memory indirection
        match idx {
            0 => self.set_b(val),
            1 => self.set_c(val),
            2 => self.set_d(val),
            3 => self.set_e(val),
            4 => self.set_h(val),
            5 => self.set_l(val),
            7 => self.set_a(val),
            _ => panic!("r8_write only valid for idx 0-5,7"),
        }
    }
    fn r16_read(&self, idx: u8) -> u16 {
        match idx {
            0 => self.bc,
            1 => self.de,
            2 => self.hl,
            3 => self.sp,
            4 => self.af,
            _ => panic!("r16_read only valid for idx 0-3"),
        }
    }
    fn r16_write(&mut self, idx: u8, val: u16) {
        match idx {
            0 => {
                self.bc = val;
            }
            1 => {
                self.de = val;
            }
            2 => {
                self.hl = val;
            }
            3 => {
                self.sp = val;
            }
            4 => {
                self.af = val;
            }
            _ => {
                panic!("r16_write only valid for idx 0-3");
            }
        }
    }
}

type Opcode = u8;
type InstructionFn<M> = fn(&mut Cpu<M>, Opcode, MCycle) -> ();
type MCycle = u8;

impl<M: MemoryIfc> Cpu<M> {
    pub fn new(mem: Rc<RefCell<M>>) -> Cpu<M> {
        Cpu {
            state: CpuState::new(),
            opcode: 0,
            dispatch: Cpu::nop,
            mcycle: 1,
            temp8: 0,
            temp16: 0,
            mem,
        }
    }

    pub fn tick(&mut self) -> () {
        match self.state.state {
            State::Running => {
                (self.dispatch)(self, self.opcode, self.mcycle);
                self.mcycle += 1;
            }
            State::Halt => {}
            State::Stop => {}
            State::Error => {}
        }

        self.state.display();
    }

    //
    // Implementation of instruction dispatch
    //

    // Helper routines

    fn mem_read(&self, addr: u16) -> u8 {
        self.mem.borrow().read(addr)
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        self.mem.borrow_mut().write(addr, val)
    }

    fn reg_8_read(&self, idx: u8) -> u8 {
        self.state.reg.r8_read(idx)
    }

    fn reg_8_write(&mut self, idx: u8, val: u8) {
        self.state.reg.r8_write(idx, val);
    }

    fn reg_16_read(&self, idx: u8) -> u16 {
        self.state.reg.r16_read(idx)
    }

    fn reg_16_write(&mut self, idx: u8, val: u16) {
        self.state.reg.r16_write(idx, val);
    }

    fn add_sub(op1: u8, mut op2: u8, sub: bool, use_carry: bool, carry_in: bool) -> (u8, u8) {
        let mut flags_out = 0;

        if use_carry && (carry_in ^ sub) {
            op2 = op2.wrapping_add(1);
        }

        if sub {
            op2 = 0u8.wrapping_sub(op2);
            // negative/subtract flag
            flags_out |= 0x40;
        }

        let ret = op1.wrapping_add(op2);

        if ret == 0 {
            // zero flag
            flags_out |= 0x80;
        }

        if ret < op1 {
            // carry flag
            flags_out |= 0x10;
        }

        if (ret & 0x0F) < (op1 & 0x0F) {
            // half-carry flag
            flags_out |= 0x20;
        }

        (ret, flags_out)
    }

    fn read_inc_pc(&mut self) -> u8 {
        let ret = self.mem_read(self.state.reg.pc);
        self.state.reg.pc += 1;
        ret
    }

    fn fetch(&mut self) {
        self.opcode = self.read_inc_pc();
        self.dispatch = Cpu::OPCODE_DISPATCH[self.opcode as usize];
        self.mcycle = 0;
    }

    //
    // instruction dispatch functions
    //
    // reasons for M-cycles:
    // * only one ALU operation per M-cycle
    // * only one memory access per M-cycle
    // * no memory access on last M-cycle of instruction (fetching next instruction)
    // * M-cycle zero is instruction fetch (overlaps with previous instruction)
    // * => M-cycles of instruction dispatch start at 1

    fn invalid(&mut self, _opcode: Opcode, _mcycle: MCycle) {
        // for invalid or yet unimplemented instructions

        self.state.state = State::Error;
        self.mcycle = 0;
    }


    // Misc / Control instructions -- complete

    fn nop(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00000000
        // Length: 1
        // M-cycles: 1
        // Flags: - - - -

        assert!(mcycle == 1);
        self.fetch();
    }

    fn stop(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00010000
        // Length: 2
        // M-cycles: 1
        // Flags: - - - -

        assert!(mcycle == 1);

        self.state.reg.pc += 1;
        self.state.state = State::Stop;

        self.fetch();
    }

    fn halt(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 01110110
        // Length: 1
        // M-cycles: 1
        // Flags: - - - -

        assert!(mcycle == 1);

        self.state.state = State::Halt;

        self.fetch();
    }

    fn prefix(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 11001011
        // Length: 1
        // M-cycles: 1
        // Flags: - - - -

        assert!(mcycle == 1);

        self.opcode = self.read_inc_pc();
        self.dispatch = Cpu::OPCODE_DISPATCH_PREFIX[self.opcode as usize];
        self.mcycle = 0;
    }

    fn di_ei(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 1111e011
        // e => 0 -> DI, 1 -> EI
        // Length: 1
        // M-cycles: 1
        // Flags: - - - -

        assert!(mcycle == 1);

        let enable = opcode >> 3 != 0;
        self.state.ime = enable;

        self.fetch();
    }


    // Jumps / Calls

    // jump_rel

    // jump_abs

    // call

    // ret

    // reset

    // reti

    // jump_hl


    // 8-bit load instructions -- complete

    fn ld_8_imm(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00rrr110
        // rrr => register / memory (HL)
        // Length: 2
        // M-cycles: 2 (r) / 3 (m)
        // Flags: - - - -

        let reg_index = opcode >> 3 & 0x07;

        if mcycle == 1 {
            self.temp8 = self.read_inc_pc();
            return;
        }

        if reg_index != 6 {
            assert!(mcycle == 2);

            self.reg_8_write(reg_index, self.temp8);

            self.fetch();
        } else {
            // reg_index == 6
            // load to memory
            match mcycle {
                2 => {
                    self.mem_write(self.state.reg.hl, self.temp8);
                }
                3 => {
                    self.fetch();
                }
                _ => {
                    panic!();
                }
            };
        }
    }

    fn ld_8(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 01xxxyyy
        // xxx => register / (HL)
        // yyy => register / (HL)
        // Length: 1
        // M-cycles 1 (rr) / 2 (rm & mr)
        // Flags: - - - -

        let dest_index = opcode >> 3 & 0x07;
        let source_index = opcode & 0x07;

        if dest_index != 6 && source_index != 6 {
            assert!(mcycle == 1);

            let val = self.reg_8_read(source_index);
            self.reg_8_write(dest_index, val);

            self.fetch();
        } else if dest_index == 6 && source_index != 6 {
            match mcycle {
                1 => {
                    let val = self.reg_8_read(source_index);
                    self.mem_write(self.state.reg.hl, val);
                }
                2 => {
                    self.fetch();
                }
                _ => panic!(),
            }
        } else if dest_index != 6 && source_index == 6 {
            match mcycle {
                1 => {
                    let val = self.mem_read(self.state.reg.hl);
                    self.reg_8_write(dest_index, val);
                }
                2 => {
                    self.fetch();
                }
                _ => panic!(),
            }
        } else {
            // dest_index == 6 && source_index == 6
            panic!();
        }
    }

    fn ld_8_rr(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 000rd010
        // r => 16-bit register (BC or DE)
        // d => direction (0 => memory write, 1 => memory read)
        // Length: 1
        // M-cycles: 2
        // Flags: - - - -
        match mcycle {
            1 => {
                let addr = self.reg_16_read(opcode >> 4 & 0x01);
                match opcode >> 3 & 0x01 {
                    0 => {
                        self.mem_write(addr, self.state.reg.a());
                    }
                    _ => {
                        self.state.reg.set_a(self.mem_read(addr));
                    }
                }
            }
            2 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn ld_8_hl(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 000id010
        // i => increment/decrement HL
        // d => direction (0 => memory write, 1 => memory read)
        // Length: 1
        // M-cycles: 2
        // Flags: - - - -
        match mcycle {
            1 => {
                match opcode >> 3 & 0x01 {
                    0 => {
                        self.mem_write(self.state.reg.hl, self.state.reg.a());
                    }
                    _ => {
                        self.state.reg.set_a(self.mem_read(self.state.reg.hl));
                    }
                }
                let inc = opcode >> 4 & 0x01 == 0;
                self.state.reg.hl = self.state.reg.hl.wrapping_add(if inc { 1 } else { (-1i16) as u16 })
            }
            2 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn ld_8_aa(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 111d1010
        // d => direction (0 => memory write, 1 => memory read)
        // Length: 3
        // M-cycles: 4
        // Flags: - - - -
        match mcycle {
            1 => {
                self.temp16 = self.read_inc_pc() as u16;
            }
            2 => {
                self.temp16 |= (self.read_inc_pc() << 8) as u16;
            }
            3 => {
                match opcode >> 4 & 0x01 {
                    0 => {
                        self.mem_write(self.temp16, self.state.reg.a());
                    }
                    _ => {
                        self.state.reg.set_a(self.mem_read(self.temp16));
                    }
                }
            }
            4 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn ldh_8(&mut self, opcode: Opcode, mut mcycle: MCycle) {
        // Opcode: 111d00a0
        // a => addr (0 => imm, 1 => C)
        // d => direction (0 => memory write, 1 => memory read)
        // Length: 1 (C) / 2 (imm)
        // M-cycles: 2 (C) / 3 (imm)
        // Flags: - - - -

        if opcode >> 1 & 0x01 == 0 {
            if mcycle == 1 {
                self.temp8 = self.read_inc_pc();
                return
            }
            else {
                mcycle -= 1;
            }
        }
        else {
            self.temp8 = self.state.reg.c();
        }
        match mcycle {
            1 => {
                let addr = 0xFF00 | self.temp8 as u16;
                match opcode >> 4 & 0x01 {
                    0 => {
                        self.mem_write(addr, self.state.reg.a());
                    }
                    _ => {
                        self.state.reg.set_a(self.mem_read(self.temp16));
                    }
                }
            }
            2 => {
                self.fetch();
            }
            _ => panic!()
        }
    }


    // 16-bit load instructions -- complete

    fn ld_16_imm(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00rr0001
        // rr => 16-bit register
        // Length: 3
        // M-cycles: 3
        // Flags: - - - -
        match mcycle {
            1 => {
                self.temp16 = self.read_inc_pc() as u16;
            }
            2 => {
                self.temp16 |= (self.read_inc_pc() as u16) << 8;
                self.reg_16_write(opcode >> 4 & 0x03, self.temp16);
            }
            3 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn push(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 11rr0101
        // rr => 16-bit register (AF instead of SP)
        // Length: 1
        // M-cycles: 4
        // Flags: - - - -
        match mcycle {
            1 => {
                let reg_index = opcode >> 4 & 0x03;
                self.temp16 = self.reg_16_read(if reg_index == 3 { 4 } else { reg_index });
                self.state.reg.sp = self.state.reg.sp.wrapping_sub(1);
            }
            2 => {
                self.mem_write(self.state.reg.sp, (self.temp16 >> 8) as u8);
                self.state.reg.sp = self.state.reg.sp.wrapping_sub(1);
            }
            3 => {
                self.mem_write(self.state.reg.sp, self.temp16 as u8);
            }
            4 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn pop(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 11rr0001
        // rr => 16-bit register (AF instead of SP)
        // Length: 1
        // M-cycles: 3
        // Flags: - - - - (rr = 3 => replace flags)
        match mcycle {
            1 => {
                self.temp16 = self.mem_read(self.state.reg.sp) as u16;
                self.state.reg.sp = self.state.reg.sp.wrapping_sub(1);
            }
            2 => {
                self.temp16 |= (self.mem_read(self.state.reg.sp) as u16) << 8;
                self.state.reg.sp = self.state.reg.sp.wrapping_sub(1);
            }
            3 => {
                let reg_index = opcode >> 4 & 0x03;
                self.reg_16_write(if reg_index == 3 { 4 } else { reg_index }, self.temp16);
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn save_sp(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00001000
        // Length: 3
        // M-cycles: 5
        // Flags: - - - -
        match mcycle {
            1 => {
                self.temp16 = self.read_inc_pc() as u16;
            }
            2 => {
                self.temp16 |= (self.read_inc_pc() as u16) << 8;
            }
            3 => {
                self.mem_write(self.temp16, self.state.reg.sp as u8);
            }
            4 => {
                self.mem_write(self.temp16.wrapping_add(1), (self.state.reg.sp >> 8) as u8);
            }
            5 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn load_sp(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 111111001
        // Length: 1
        // M-cycles: 2
        // Flags: - - - -
        match mcycle {
            1 => {
                self.state.reg.sp = self.state.reg.hl;
            }
            2 => {
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn sp_offs(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 111111000
        // Length: 2
        // M-cycles: 3
        // Flags: 0 0 H C
        match mcycle {
            1 => {
                self.temp8 = self.read_inc_pc();
            }
            2 => {
                let (low, flags) = Self::add_sub(self.state.reg.sp as u8, self.temp8, false, false, false);
                self.state.reg.set_l(low);
                self.state.reg.set_f(flags);
            }
            3 => {
                let (high, flags) = Self::add_sub((self.state.reg.sp >> 8) as u8, 0, false, true, self.state.reg.cf());
                self.state.reg.set_h(high);
                self.state.reg.update_flags(0x00, 0xC0, flags, 0x30);
                self.fetch();
            }
            _ => panic!()
        }
    }


    // 8-bit ALU -- complete
    fn inc_dec_8(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00rrr10d
        // rrr => register / memory (HL)
        // d => 0 -> inc, 1 -> dec
        // Length: 1
        // M-cycles: 1 (r) / 3 (m)
        // Flags: Z 0 H - (inc) / Z 1 H - (dec)

        let dec = (opcode & 0x01) != 0;
        let reg_index = (opcode >> 3) & 0x07;

        if reg_index != 6 {
            assert!(mcycle == 1);

            let (val, flags) = Self::add_sub(
                self.reg_8_read(reg_index),
                1,
                dec,
                false,
                self.state.reg.cf(),
            );
            self.reg_8_write(reg_index, val);
            self.state.reg.update_flags(
                if dec { 0x40 } else { 0x00 },
                if dec { 0x00 } else { 0x40 },
                flags,
                0xA0, // zero and half-carry flags
            );

            self.fetch();
        } else {
            // reg_index == 6
            match mcycle {
                1 => {
                    let (val, flags) = Self::add_sub(
                        self.mem_read(self.state.reg.hl),
                        1,
                        dec,
                        false,
                        self.state.reg.cf(),
                    );
                    self.temp8 = val;
                    self.state.reg.update_flags(
                        if dec { 0x40 } else { 0x00 },
                        if dec { 0x00 } else { 0x40 },
                        flags,
                        0xA0, // zero and half-carry flags
                    );
                }
                2 => {
                    self.mem_write(self.state.reg.hl, self.temp8);
                }
                3 => {
                    self.fetch();
                }
                _ => {
                    panic!();
                }
            }
        }
    }

    fn alu(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 10ooorrr / 11ooo110 (imm)
        // ooo => operation
        // rrr => register / memory (HL)
        // Length: 1 / 2 (imm)
        // M-cycles: 1 (r) / 2 (m, imm)
        // Flags: Z N H C (depending on op)

        let op = opcode >> 3 & 0x07;
        let reg_index = if opcode >> 6 == 0x02 {
            opcode & 0x07
        } else {
            8
        };

        if reg_index == 6 {
            if mcycle == 1 {
                self.temp8 = self.mem_read(self.state.reg.hl);
                return;
            }
            assert!(mcycle == 2);
        }
        else if reg_index == 8 {
            if mcycle == 1 {
                self.temp8 = self.read_inc_pc();
                return;
            }
            assert!(mcycle == 2);
        } else {
            assert!(mcycle == 1);
            self.temp8 = self.reg_8_read(reg_index);
        }

        let a = self.state.reg.a();
        let carry_in = self.state.reg.cf();
        match op {
            0 => {
                // ADD
                let (val, flags) = Self::add_sub(a, self.temp8, false, false, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            1 => {
                // ADC
                let (val, flags) = Self::add_sub(a, self.temp8, false, true, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            2 => {
                // SUB
                let (val, flags) = Self::add_sub(a, self.temp8, true, false, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            3 => {
                // SBC
                let (val, flags) = Self::add_sub(a, self.temp8, true, true, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            4 => {
                // AND
                let val = a & self.temp8;
                self.state.reg.set_a(val);
                self.state
                    .reg
                    .update_flags(0x20, 0x50, if val == 0 { 0x80 } else { 0 }, 0x80);
            }
            5 => {
                // XOR
                let val = a ^ self.temp8;
                self.state.reg.set_a(val);
                self.state
                    .reg
                    .update_flags(0, 0x70, if val == 0 { 0x80 } else { 0 }, 0x80);
            }
            6 => {
                // OR
                let val = a | self.temp8;
                self.state.reg.set_a(val);
                self.state
                    .reg
                    .update_flags(0, 0x70, if val == 0 { 0x80 } else { 0 }, 0x80);
            }
            7 => {
                // CP
                let (_val, flags) = Self::add_sub(a, self.temp8, true, false, carry_in);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            _ => {
                panic!();
            }
        }

        self.fetch();
    }

    fn daa(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00100111
        // Length: 1
        // M-cycles: 1
        // Flags: Z - 0 C
        assert!(mcycle == 1);

        let mut a = self.state.reg.a();
        if self.state.reg.hf() || a & 0x0F >= 0x0A {
            // adjust low nybble
            a = a.wrapping_add(0x06);
        }
        if self.state.reg.cf() || a & 0xF0 >= 0xA0 {
            // adjust high nybble
            a = a.wrapping_add(0x60);
            self.state.reg.update_flags(0x10, 0x00, 0x00, 0x00)
        }

        self.state.reg.update_flags(if a == 0 { 0x80 } else { 0x00 }, 0x20, 0x00, 0x00);
        self.state.reg.set_a(a);

        self.fetch();
    }

    fn cpl(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00101111
        // Length: 1
        // M-cycles: 1
        // Flags: - 1 1 -
        assert!(mcycle == 1);

        self.state.reg.set_a(!self.state.reg.a());
        self.state.reg.update_flags(0x60, 0x00, 0x00, 0x00);

        self.fetch();
    }

    fn scf(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00110111
        // Length: 1
        // M-cycles: 1
        // Flags: - 0 0 1
        assert!(mcycle == 1);

        self.state.reg.update_flags(0x10, 0x60, 0x00, 0x00);

        self.fetch();
    }

    fn ccf(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00111111
        // Length: 1
        // M-cycles: 1
        // Flags: - 0 0 C
        assert!(mcycle == 1);

        self.state.reg.update_flags(0x00, 0x60, !self.state.reg.f(), 0x10);

        self.fetch();
    }


    // 16-bit ALU -- complete

    fn inc_dec_16(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00rrd011
        // rr => register
        // d => 0 -> inc, 1 -> dec
        // Length: 1
        // M-cycles: 2
        // Flags: - - - -

        let dec = (opcode & 0x08) != 0;
        let reg_index = (opcode >> 4) & 0x03;

        match mcycle {
            1 => {
                self.temp8 = self.state.reg.f();
                self.temp16 = self.reg_16_read(reg_index);
                let (val, flags) = Self::add_sub(self.temp16 as u8, 1, dec, false, self.state.reg.cf());
                self.temp16 = (self.temp16 & 0xFF00) | val as u16;
                self.state.reg.set_f(flags);
            }
            2 => {
                let (val, _) = Self::add_sub((self.temp16 >> 8) as u8, 0, dec, true, self.state.reg.cf());
                self.reg_16_write(reg_index, self.temp16 & 0x00FF | (val as u16) << 8);
                self.state.reg.set_f(self.temp8);

                self.fetch();
            }
            _ => {
                panic!();
            }
        }
    }

    fn add_hl(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 000rr1001
        // rr => 16-bit register
        // Length: 1
        // M-cycles: 2
        // Flags: - 0 H C
        let reg_index = opcode >> 4 & 0x03;
        match mcycle {
            1 => {
                let (low, flags) = Self::add_sub(self.state.reg.l(), self.reg_16_read(reg_index) as u8, false, false, false);
                self.state.reg.set_l(low);
                self.state.reg.update_flags(0x00, 0x40, flags, 0x30);
            }
            2 => {
                let (high, flags) = Self::add_sub(self.state.reg.h(), (self.reg_16_read(reg_index) >> 8) as u8, false, true, self.state.reg.cf());
                self.state.reg.set_h(high);
                self.state.reg.update_flags(0x00, 0x40, flags, 0x30);
                self.fetch();
            }
            _ => panic!()
        }
    }

    fn add_sp(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 111101000
        // Length: 2
        // M-cycles: 4
        // Flags: 0 0 H C
        match mcycle {
            1 => {
                self.temp8 = self.read_inc_pc();
            }
            2 => {
                let (low, flags) = Self::add_sub(self.state.reg.sp as u8, self.temp8, false, false, false);
                self.state.reg.sp = self.state.reg.sp & 0xFF00 | low as u16;
                self.state.reg.set_f(flags);
            }
            3 => {
                let (high, flags) = Self::add_sub((self.state.reg.sp >> 8) as u8, 0, false, true, self.state.reg.cf());
                self.state.reg.sp = self.state.reg.sp & 0x00FF | (high as u16) << 8;
                self.state.reg.update_flags(0x00, 0xC0, flags, 0x30);
            }
            4 => {
                self.fetch();
            }
            _ => panic!()
        }
    }


    // 8-bit shift, rotate, bit
    // ...

    #[cfg_attr(rustfmt, rustfmt_skip)]
    const OPCODE_DISPATCH: [InstructionFn<M>; 256] = [
        Cpu::nop,     Cpu::ld_16_imm, Cpu::ld_8_rr, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::save_sp, Cpu::add_hl,    Cpu::ld_8_rr, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::stop,    Cpu::ld_16_imm, Cpu::ld_8_rr, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::add_hl,    Cpu::ld_8_rr, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::ld_16_imm, Cpu::ld_8_hl, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::daa,
        Cpu::invalid, Cpu::add_hl,    Cpu::ld_8_hl, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::cpl,
        Cpu::invalid, Cpu::ld_16_imm, Cpu::ld_8_hl, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::scf,
        Cpu::invalid, Cpu::add_hl,    Cpu::ld_8_hl, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::ccf,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::halt,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,      Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,       Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::invalid, Cpu::pop,       Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::push,      Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid,   Cpu::invalid, Cpu::prefix,     Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::pop,       Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::push,      Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid,   Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::ldh_8,   Cpu::pop,       Cpu::ldh_8,   Cpu::invalid,    Cpu::invalid,   Cpu::push,      Cpu::alu,      Cpu::invalid,
        Cpu::add_sp,  Cpu::invalid,   Cpu::ld_8_aa, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::ldh_8,   Cpu::pop,       Cpu::ldh_8,   Cpu::di_ei,      Cpu::invalid,   Cpu::push,      Cpu::alu,      Cpu::invalid,
        Cpu::sp_offs, Cpu::load_sp,   Cpu::ld_8_aa, Cpu::di_ei,      Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
    ];
    
    #[cfg_attr(rustfmt, rustfmt_skip)]
    const OPCODE_DISPATCH_PREFIX: [InstructionFn<M>; 256] = [
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,
    ];
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashMap;
    
    #[derive(Clone, PartialEq, Eq, Debug)]
    struct SparseMem (HashMap<u16, u8>);

    fn with_address(instr: &[u8]) -> impl Iterator<Item = (u16, u8)> + '_ {
        (0..).zip(instr.iter().copied())
    }

    impl<I> From<I> for SparseMem
        where I : IntoIterator<Item=(u16, u8)>
    {
        fn from(iter: I) -> SparseMem {
            SparseMem(HashMap::from_iter(iter))
        }
    }

    impl MemoryIfc for SparseMem {
        fn read(&self, addr: u16) -> u8 {
            *self.0.get(&addr).unwrap_or(&0)
        }

        fn write(&mut self, addr: u16, val: u8) {
            self.0.insert(addr, val);
        }
    }

    fn register_test(instr: &[u8], mut expected_state: CpuState) {
        let mem = Rc::new(RefCell::new(SparseMem::from(with_address(instr))));
        let mut cpu = Cpu::new(mem);

        while cpu.state.state == State::Running {
            cpu.tick();
        }

        expected_state.reg.pc = instr.len() as u16 + 1;
        expected_state.state = match instr[instr.len()-1] {
            0x10 => State::Stop,
            0x76 => State::Halt,
            _ => State::Error,
        };

        assert_eq!(cpu.state, expected_state);
    }

    fn memory_test(instr: &[u8], mut expected: SparseMem) {
        let mem = Rc::new(RefCell::new(SparseMem::from(with_address(instr))));
        let mut cpu = Cpu::new(mem.clone());

        while cpu.state.state == State::Running {
            cpu.tick();
        }

        expected.0.extend(with_address(instr));

        assert_eq!(expected, *mem.borrow());
    }

    // Misc / Control instructions


    // Jumps / Calls


    // 8-bit load instructions

    #[test]
    fn load_immediate_8_test() {
        {
            let mut state = CpuState::new();
            state.reg.set_b(0xAA);
            // LD B 0xAA
            register_test(&[0x06, 0xAA, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_c(0xAA);
            register_test(&[0x0E, 0xAA, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_d(0xAA);
            register_test(&[0x16, 0xAA, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_e(0xAA);
            register_test(&[0x1E, 0xAA, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_h(0xAA);
            register_test(&[0x26, 0xAA, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_l(0xAA);
            register_test(&[0x2E, 0xAA, 0x76], state);
        }
        {
            let mem = SparseMem(HashMap::from([(0xFFFF, 0xAA)]));
            // DEC HL
            // LD (HL) 0xAA
            memory_test(&[0x2B, 0x36, 0xAA, 0x76], mem);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_a(0xAA);
            register_test(&[0x3E, 0xAA, 0x76], state);
        }
    }

    #[test]
    fn load_register_8_test() {
        for i in 0..8 {
            if i == 6 { continue; }
            for j in 0..8 {
                if j == 6 { continue; }
                let mut state = CpuState::new();
                state.reg.r8_write(i, 0xAA);
                state.reg.r8_write(j, 0xAA);
                let prep_instr = 0x06 | i << 3;
                let load_instr = 0x40 | j << 3 | i;
                // LD r1 0xAA
                // LD r2 r1
                register_test(&[prep_instr, 0xAA, load_instr, 0x76], state);
            }
            {
                let mut state = CpuState::new();
                state.reg.r8_write(i, 0xAA);
                state.reg.set_f(0x80);
                let load_instr = 0x40 | i << 3 | 6;
                // 0xAA (XOR D) => dummy instruction / payload for LD r (HL)
                // LD r (HL) => HL = 0 => LD r 0xAA
                register_test(&[0xAA, load_instr, 0x76], state);
            }
            {
                let addr = if i == 4 { 0xAAAA } else { 0x00AA };
                let mem = SparseMem(HashMap::from([(addr, 0xAA)]));
                let prep_instr = 0x06 | i << 3;
                let load_instr = 0x40 | 6 << 3 | i;
                // LD L 0xAA
                // LD r 0xAA
                // LD (HL) r
                memory_test(&[0x2E, 0xAA, prep_instr, 0xAA, load_instr, 0x76], mem);
            }
        }
    }

    // load_memory_8_test()


    // 16-bit load instructions

    #[test]
    fn load_immediate_16_test() {
        {
            let mut state = CpuState::new();
            state.reg.bc = 0xDEAD;
            // LD rr 0xDEAD
            register_test(&[0x01, 0xAD, 0xDE, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.de = 0xDEAD;
            register_test(&[0x11, 0xAD, 0xDE, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.hl = 0xDEAD;
            register_test(&[0x21, 0xAD, 0xDE, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.sp = 0xDEAD;
            register_test(&[0x31, 0xAD, 0xDE, 0x76], state);
        }
    }

    // stack_test()


    // 8-bit ALU

    #[test]
    fn inc_dec_8_test() {
        {
            let mut state = CpuState::new();
            state.reg.set_b(0x01);
            // INC B
            register_test(&[0x04, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_b(0xFF);
            state.reg.set_f(0x40);
            // DEC B
            register_test(&[0x05, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            // LD B 0xFF
            // INC B
            register_test(&[0x06, 0xFF, 0x04, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            // LD B 0x01
            // DEC B
            register_test(&[0x06, 0x01, 0x05, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_c(0x01);
            register_test(&[0x0C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_c(0xFF);
            state.reg.set_f(0x40);
            register_test(&[0x0D, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            register_test(&[0x0E, 0xFF, 0x0C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            register_test(&[0x0E, 0x01, 0x0D, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_d(0x01);
            register_test(&[0x14, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_d(0xFF);
            state.reg.set_f(0x40);
            register_test(&[0x15, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            register_test(&[0x16, 0xFF, 0x14, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            register_test(&[0x16, 0x01, 0x15, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_e(0x01);
            register_test(&[0x1C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_e(0xFF);
            state.reg.set_f(0x40);
            register_test(&[0x1D, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            register_test(&[0x1E, 0xFF, 0x1C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            register_test(&[0x1E, 0x01, 0x1D, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_h(0x01);
            register_test(&[0x24, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_h(0xFF);
            state.reg.set_f(0x40);
            register_test(&[0x25, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            register_test(&[0x26, 0xFF, 0x24, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            register_test(&[0x26, 0x01, 0x25, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_l(0x01);
            register_test(&[0x2C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_l(0xFF);
            state.reg.set_f(0x40);
            register_test(&[0x2D, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            register_test(&[0x2E, 0xFF, 0x2C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            register_test(&[0x2E, 0x01, 0x2D, 0x76], state);
        }
        {
            let mem = SparseMem(HashMap::from([(0xFFFF, 0x01), (0xFFFE, 0xFF), (0xFFFD, 0), (0xFFFC, 0)]));
            // DEC HL
            // INC (HL)
            //
            // DEC HL
            // DEC (HL)
            //
            // DEC HL
            // LD (HL) 0xFF
            // INC (HL)
            //
            // DEC HL
            // LD (HL) 0x01
            // DEC (HL)
            memory_test(&[0x2B, 0x34, 0x2B, 0x35, 0x2B, 0x36, 0xFF, 0x34, 0x2B, 0x36, 0x01, 0x35, 0x76], mem);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_a(0x01);
            register_test(&[0x3C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_a(0xFF);
            state.reg.set_f(0x40);
            register_test(&[0x3D, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xA0);
            register_test(&[0x3E, 0xFF, 0x3C, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_f(0xE0);
            register_test(&[0x3E, 0x01, 0x3D, 0x76], state);
        }
    }

    #[test]
    fn alu_test() {
        // test that registers are handled properly
        for i in 0..8 {
            for op in 0..8 {
                {
                    let mut state = CpuState::new();
                    state.reg.set_a(match op {
                        0 | 1 => std::ops::Add::add,
                        2 => std::ops::Sub::sub,
                        3 => |a: u8, b:u8| a.wrapping_sub(b).wrapping_sub(1),
                        4 => std::ops::BitAnd::bitand,
                        5 => std::ops::BitXor::bitxor,
                        6 => std::ops::BitOr::bitor,
                        7 => |a: u8, _| a,
                        _ => panic!()
                    }(if i != 7 { 0x42 } else { 0x2B }, 0x2B));
                    state.reg.set_f(match op {
                        0 | 1 => if i == 7 { 0x20 } else { 0x00 },
                        2 => if i == 7 { 0xF0 } else { 0x50 },
                        3 => if i == 7 { 0x40 } else { 0x50 },
                        4 => 0x20,
                        5 => if i == 7 { 0x80 } else { 0x00 },
                        6 => 0x00,
                        7 => if i == 7 { 0xF0 } else { 0x50 },
                        _ => panic!()
                    });
                    let prep_instr = 0x06 | i << 3;
                    let alu_instr = 0x80 | op << 3 | i;
                    if i != 6 {
                        if i != 7 {
                            state.reg.r8_write(i, 0x2B);
                        }
                        // LD A 0x42
                        // LD r 0x2B
                        // alu A r
                        register_test(&[0x3E, 0x42, prep_instr, 0x2B, alu_instr, 0x76], state);
                    } else {
                        state.reg.hl = 0xFFFF;
                        // LD A 0x42
                        // DEC HL
                        // LD (HL) 0x2B
                        // alu A (HL)
                        register_test(&[0x3E, 0x42, 0x2B, prep_instr, 0x2B, alu_instr, 0x76], state);
                    }
                }
            }
        }

        // test multi-byte addition (ADD + ADC with carry)
        {
            let bc = 0xDEAD;
            let de = 0x12BE;
            let hl = bc + de;
            let mut state = CpuState::new();
            state.reg.bc = bc;
            state.reg.de = de;
            state.reg.hl = hl;
            state.reg.set_a(state.reg.h());
            state.reg.set_f(0x20);
            // LD BC 0x1248
            // LD DE 0x3141
            // LD A C
            // ADD A E
            // LD L A
            // LD A B
            // ADC A D
            // LD H A
            register_test(&[0x01, 0xAD, 0xDE, 0x11, 0xBE, 0x12, 0x79, 0x83, 0x6F, 0x78, 0x8A, 0x67, 0x76], state);
        }
        // test multi-byte subtraction (SUB + SBC with borrow)
        {
            let bc = 0xDEAD;
            let de = 0x12BE;
            let hl = bc - de;
            let mut state = CpuState::new();
            state.reg.bc = bc;
            state.reg.de = de;
            state.reg.hl = hl;
            state.reg.set_a(state.reg.h());
            state.reg.set_f(0x70);
            // LD BC 0x1248
            // LD DE 0x3141
            // LD A C
            // SUB A E
            // LD L A
            // LD A B
            // SBC A D
            // LD H A
            register_test(&[0x01, 0xAD, 0xDE, 0x11, 0xBE, 0x12, 0x79, 0x93, 0x6F, 0x78, 0x9A, 0x67, 0x76], state);
        }
        // test multi-byte subtraction (SUB + SBC without borrow)
        {
            let bc = 0xDEAD;
            let de = 0x1248;
            let hl = bc - de;
            let mut state = CpuState::new();
            state.reg.bc = bc;
            state.reg.de = de;
            state.reg.hl = hl;
            state.reg.set_a(state.reg.h());
            state.reg.set_f(0x70);
            // LD BC 0x1248
            // LD DE 0x3141
            // LD A C
            // SUB A E
            // LD L A
            // LD A B
            // SBC A D
            // LD H A
            register_test(&[0x01, 0xAD, 0xDE, 0x11, 0x48, 0x12, 0x79, 0x93, 0x6F, 0x78, 0x9A, 0x67, 0x76], state);
        }
        // test scf
        {
            //TODO
        }
        // test ccf
        {
            //TODO
        }
        // test cpl
        {
            let mut state = CpuState::new();
            state.reg.set_a(0x55);
            state.reg.set_f(0x60);
            register_test(&[0x3E, 0xAA, 0x2F, 0x76], state);
        }
    }

    // bcd_test()


    // 16-bit ALU

    #[test]
    fn inc_dec_16_test() {
        {
            let mut state = CpuState::new();
            state.reg.bc = 0x0001;
            // INC BC
            register_test(&[0x03, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.bc = 0xFFFF;
            // DEC BC
            register_test(&[0x0B, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.de = 0x0001;
            // INC DE
            register_test(&[0x13, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.de = 0xFFFF;
            // DEC DE
            register_test(&[0x1B, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.hl = 0x0001;
            // INC HL
            register_test(&[0x23, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.hl = 0xFFFF;
            // DEC HL
            register_test(&[0x2B, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.sp = 0x0001;
            // INC SP
            register_test(&[0x33, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.sp = 0xFFFF;
            // DEC SP
            register_test(&[0x3B, 0x76], state);
        }
    }

    // 8-bit shift, rotate, bit

}
