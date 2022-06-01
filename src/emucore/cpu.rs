use super::mem::MemoryController;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Cpu<'a> {
    state: CpuState,
    mem: Rc<RefCell<MemoryController<'a>>>,
    opcode: Opcode,
    dispatch: InstructionFn<'a>,
    mcycle: MCycle,
    temp8: u8,
    temp16: u16,
}

impl<'a> Cpu<'a> {
    pub fn new(mem: Rc<RefCell<MemoryController>>) -> Cpu {
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
        println!("B: {:#x} C: {:#x} BC: {:#x}", b, c, self.reg.bc);
        let d = self.reg.d();
        let e = self.reg.e();
        println!("D: {:#x} E: {:#x} DE: {:#x}", d, e, self.reg.de);
        let h = self.reg.h();
        let l = self.reg.l();
        println!("H: {:#x} L: {:#x} HL: {:#x}", h, l, self.reg.hl);
        let a = self.reg.a();
        let f = self.reg.f();
        println!("A: {:#x} F: {:#x} AF: {:#x}", a, f, self.reg.af);
        println!("SP: {:#x}", self.reg.sp);
        println!("PC: {:#x}", self.reg.pc);
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
        self.bc &= 0xFF00;
        self.bc |= (val & 0xF0) as u16;
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
}

type Opcode = u8;
type MCycle = u8;
type InstructionFn<'a> = fn(&mut Cpu<'a>, Opcode, MCycle) -> ();

// Implementation of instruction dispatch
impl<'a> Cpu<'a> {
    //
    // Helper routines
    //

    fn mem_read(&mut self, addr: u16) -> u8 {
        self.mem.borrow().read(addr)
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        self.mem.borrow_mut().write(addr, val)
    }

    fn reg_8_read(&self, idx: u8) -> u8 {
        // idx == 6 is invalid, usually used by (HL) memory indirection
        match idx {
            0 => self.state.reg.b(),
            1 => self.state.reg.c(),
            2 => self.state.reg.d(),
            3 => self.state.reg.e(),
            4 => self.state.reg.h(),
            5 => self.state.reg.l(),
            7 => self.state.reg.a(),
            _ => panic!(),
        }
    }

    fn reg_8_write(&mut self, idx: u8, val: u8) {
        // idx == 6 is invalid, usually used by (HL) memory indirection
        match idx {
            0 => self.state.reg.set_b(val),
            1 => self.state.reg.set_c(val),
            2 => self.state.reg.set_d(val),
            3 => self.state.reg.set_e(val),
            4 => self.state.reg.set_h(val),
            5 => self.state.reg.set_l(val),
            7 => self.state.reg.set_a(val),
            _ => panic!(),
        }
    }

    fn reg_16_read(&self, idx: u8) -> u16 {
        match idx {
            0 => self.state.reg.bc,
            1 => self.state.reg.de,
            2 => self.state.reg.hl,
            3 => self.state.reg.sp,
            _ => panic!(),
        }
    }

    fn reg_16_write(&mut self, idx: u8, val: u16) {
        match idx {
            0 => {
                self.state.reg.bc = val;
            }
            1 => {
                self.state.reg.de = val;
            }
            2 => {
                self.state.reg.hl = val;
            }
            3 => {
                self.state.reg.sp = val;
            }
            _ => {
                panic!();
            }
        }
    }

    fn add_sub(op1: u8, mut op2: u8, sub: bool, use_carry: bool, carry_in: bool) -> (u8, u8) {
        let mut flags_out = 0;

        if use_carry && carry_in {
            op2 += 1;
        }

        if sub {
            op2 = 0 - op2;
            // negative/subtract flag
            flags_out |= 0x40;
        }

        let ret = op1 + op2;

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

    fn nop(&mut self, _opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00000000
        // Length: 1
        // M-cycles: 1
        // Flags: - - - -

        assert!(mcycle == 1);
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

            let (val, flags) = Cpu::add_sub(
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
                    let (val, flags) = Cpu::add_sub(
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

    fn inc_dec_16(&mut self, opcode: Opcode, mcycle: MCycle) {
        // Opcode: 00rrd011
        // rr => register
        // d => 0 -> inc, 1 -> dec
        // Length: 1
        // M-cycles: 2
        // Flags: - - - -

        let dec = (opcode & 0x08) == 0;
        let reg_index = (opcode >> 4) & 0x03;

        match mcycle {
            1 => {
                self.temp8 = self.state.reg.f();
                self.temp16 = self.reg_16_read(reg_index);
                let (val, flags) =
                    Cpu::add_sub(self.temp16 as u8, 1, dec, false, self.state.reg.cf());
                self.temp16 = (self.temp16 & 0xFF00) | val as u16;
                self.state.reg.set_f(flags);
            }
            2 => {
                let (val, _) =
                    Cpu::add_sub((self.temp16 >> 8) as u8, 0, dec, true, self.state.reg.cf());
                self.reg_16_write(reg_index, self.temp16 & 0x00FF | (val as u16) << 8);
                self.state.reg.set_f(self.temp8);

                self.fetch();
            }
            _ => {
                panic!();
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
            0x08
        };

        if reg_index == 6 {
            if mcycle == 1 {
                self.temp8 = self.mem_read(self.state.reg.hl);
                return;
            }
            assert!(mcycle == 2);
        }
        if reg_index == 8 {
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
                let (val, flags) = Cpu::add_sub(a, self.temp8, false, false, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            1 => {
                // ADC
                let (val, flags) = Cpu::add_sub(a, self.temp8, false, true, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            2 => {
                // SUB
                let (val, flags) = Cpu::add_sub(a, self.temp8, true, false, carry_in);
                self.state.reg.set_a(val);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            3 => {
                // SBC
                let (val, flags) = Cpu::add_sub(a, self.temp8, true, true, carry_in);
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
                let (_val, flags) = Cpu::add_sub(a, self.temp8, true, false, carry_in);
                self.state.reg.update_flags(0, 0, flags, 0xF0);
            }
            _ => {
                panic!();
            }
        }

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


    #[cfg_attr(rustfmt, rustfmt_skip)]
    const OPCODE_DISPATCH: [InstructionFn<'a>; 256] = [
        Cpu::nop,     Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::stop,    Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::inc_dec_16, Cpu::inc_dec_8, Cpu::inc_dec_8, Cpu::ld_8_imm, Cpu::invalid,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::halt,     Cpu::ld_8,
        Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,    Cpu::ld_8,       Cpu::ld_8,      Cpu::ld_8,      Cpu::ld_8,     Cpu::ld_8,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::alu,     Cpu::alu,     Cpu::alu,     Cpu::alu,        Cpu::alu,       Cpu::alu,       Cpu::alu,      Cpu::alu,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::prefix,     Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::invalid,    Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::di_ei,      Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
        Cpu::invalid, Cpu::invalid, Cpu::invalid, Cpu::di_ei,      Cpu::invalid,   Cpu::invalid,   Cpu::alu,      Cpu::invalid,
    ];
    
    #[cfg_attr(rustfmt, rustfmt_skip)]
    const OPCODE_DISPATCH_PREFIX: [InstructionFn<'a>; 256] = [
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
    use super::super::mem::*;

    fn register_test(instr: &[u8], mut expected_state: CpuState) {
        let cart = Box::new(Cartridge::new(instr));
        let mem = Rc::new(RefCell::new(MemoryController::new(cart)));
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

    #[test]
    fn load_immediate_test() {
        {
            let mut state = CpuState::new();
            state.reg.set_a(0xAA);
            register_test(&[0x3E, 0xAA, 0x76], state);
        }
        {
            let mut state = CpuState::new();
            state.reg.set_b(0xAA);
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
    }
}
