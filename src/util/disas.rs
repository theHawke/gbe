use crate::emucore::mem::MemoryIfc;

pub struct MemoryIterator<'a, M> {
    mem: &'a M,
    addr: u16,
}

impl<'a, M: MemoryIfc> Iterator for MemoryIterator<'a, M> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.mem.read(self.addr);
        self.addr = self.addr.wrapping_add(1);
        Some(ret)
    }
}

pub fn at_addr<'a, M: MemoryIfc>(mem: &'a M, addr: u16) -> MemoryIterator<'a, M> {
    MemoryIterator {
        mem,
        addr,
    }
}


pub fn disassemble_one(bytes: &mut impl Iterator<Item = u8>) -> Option<String> {
    let opcode = bytes.next()?;
    let (instr, byte2, byte3): (String, _, _) = match &INSTRUCTIONS[opcode as usize] {
        &NoArg(m) => (m.to_owned(), None, None),
        &OneArg(m, a) => {
            let (arg, byte2, byte3) = get_arg(bytes, a)?;
            (format!("{} {}", m, arg), byte2, byte3)
        }
        &TwoArg(m, a1, a2) => {
            let (arg1, byte21, byte31) = get_arg(bytes, a1)?;
            let (arg2, byte22, byte32) = get_arg(bytes, a2)?;
            (format!("{} {}, {}", m, arg1, arg2), byte21.or(byte22), byte31.or(byte32))
        }
        &Prefix => {
            let byte = bytes.next()?;
            (get_prefix(byte), Some(byte), None)
        },
        &Invalid => ("Invalid Instr".to_owned(), None, None), 
    };
    match (byte2, byte3) {
        (None, None) => Some(format!("{:02x}        {}", opcode, instr)),
        (Some(b2), None) => Some(format!("{:02x} {:02x}     {}", opcode, b2, instr)),
        (Some(b2), Some(b3)) => Some(format!("{:02x} {:02x} {:02x}  {}", opcode, b2, b3, instr)),
        _ => None,
    }
}

fn get_arg(bytes: &mut impl Iterator<Item = u8>, a: Arg) -> Option<(String, Option<u8>, Option<u8>)> {
    Some(match a {
        Fix(c) => (c.to_owned(), None, None),
        Imm8 => {
            let byte2 = bytes.next()?;
            (format!("${:02X}", byte2), Some(byte2), None)
        }
        Imm16 => {
            let byte2 = bytes.next()?;
            let byte3 = bytes.next()?;
            (format!("${1:02X}{0:02X}", byte2, byte3), Some(byte2), Some(byte3))
        }
        Addr8 => {
            let byte2 = bytes.next()?;
            (format!("(${:02X})", byte2), Some(byte2), None)
        }
        Addr16 => {
            let byte2 = bytes.next()?;
            let byte3 = bytes.next()?;
            (format!("(${1:02X}{0:02X})", byte2, byte3), Some(byte2), Some(byte3))
        }
        Rel8 => {
            let byte2 = bytes.next()?;
            (format!("{}", byte2 as i8), Some(byte2), None)
        }
    })
}

fn get_prefix(opcode: u8) -> String {
    let reg = ["B", "C", "D", "E", "H", "L", "(HL)", "A"][(opcode & 0x07) as usize];
    let instr = match opcode >> 3 {
        0 => "RLC".to_owned(),
        1 => "RRC".to_owned(), 
        2 => "RL".to_owned(),
        3 => "RR".to_owned(),
        4 => "SLA".to_owned(),
        5 => "SRA".to_owned(),
        6 => "SWAP".to_owned(),
        7 => "SRL".to_owned(),
        b @ 8..=15 => format!("BIT {},", b & 0x07),
        b @ 16..=23 => format!("RES {},", b & 0x07),
        b @ 24..=31 => format!("SET {},", b & 0x07),
        _ => panic!(),
    };
    format!("{} {}", instr, reg)
}

#[derive(Clone, Copy)]
enum Arg {
    Fix(&'static str),
    Imm8,
    Imm16,
    Addr8,
    Addr16,
    Rel8,
}

const B: Arg = Fix("B");
const C: Arg = Fix("C");
const D: Arg = Fix("D");
const E: Arg = Fix("E");
const H: Arg = Fix("H");
const L: Arg = Fix("L");
const A: Arg = Fix("A");
const BC: Arg = Fix("BC");
const DE: Arg = Fix("DE");
const HL: Arg = Fix("HL");
const SP: Arg = Fix("SP");
const AF: Arg = Fix("AF");
const NZ: Arg = Fix("NZ");
const Z: Arg = Fix("Z");
const NC: Arg = Fix("NC");
const MHL: Arg = Fix("(HL)");
const MC: Arg = Fix("(C)");
const MBC: Arg = Fix("(BC)");
const MDE: Arg = Fix("(DE)");

enum Instr {
    NoArg(&'static str),
    OneArg(&'static str, Arg),
    TwoArg(&'static str, Arg, Arg),
    Prefix,
    Invalid,
}

use Instr::*;
use Arg::*;

const INSTRUCTIONS: [Instr; 256] = [
    NoArg("NOP"),             TwoArg("LD", BC, Imm16), TwoArg("LD", MBC, A),     OneArg("INC", BC),    OneArg("INC", B),           OneArg("DEC", B),       TwoArg("LD", B, Imm8),   NoArg("RLCA"),
    TwoArg("LD", Addr16, SP), TwoArg("ADD", HL, BC),   TwoArg("LD", A, MBC),     OneArg("DEC", BC),    OneArg("INC", C),           OneArg("DEC", C),       TwoArg("LD", C, Imm8),   NoArg("RRCA"),
    OneArg("STOP", Imm8),     TwoArg("LD", DE, Imm16), TwoArg("LD", MDE, A),     OneArg("INC", DE),    OneArg("INC", D),           OneArg("DEC", D),       TwoArg("LD", D, Imm8),   NoArg("RLA"),
    OneArg("JR", Rel8),       TwoArg("ADD", HL, DE),   TwoArg("LD", A, MDE),     OneArg("DEC", DE),    OneArg("INC", E),           OneArg("DEC", E),       TwoArg("LD", E, Imm8),   NoArg("RRA"),
    TwoArg("JR", NZ, Rel8),   TwoArg("LD", HL, Imm16), TwoArg("LDI", MHL, A),    OneArg("INC", HL),    OneArg("INC", H),           OneArg("DEC", H),       TwoArg("LD", H, Imm8),   NoArg("DAA"),
    TwoArg("JR", Z, Rel8),    TwoArg("ADD", HL, HL),   TwoArg("LDI", A, MHL),    OneArg("DEC", HL),    OneArg("INC", L),           OneArg("DEC", L),       TwoArg("LD", L, Imm8),   NoArg("CPL"),
    TwoArg("JR", NC, Rel8),   TwoArg("LD", SP, Imm16), TwoArg("LDD", MHL, A),    OneArg("INC", SP),    OneArg("INC", MHL),         OneArg("DEC", MHL),     TwoArg("LD", MHL, Imm8), NoArg("SCF"),
    TwoArg("JR", C, Rel8),    TwoArg("ADD", HL, SP),   TwoArg("LDD", A, MHL),    OneArg("DEC", SP),    OneArg("INC", A),           OneArg("DEC", A),       TwoArg("LD", A, Imm8),   NoArg("CCF"),
    TwoArg("LD", B, B),       TwoArg("LD", B, C),      TwoArg("LD", B, D),       TwoArg("LD", B, E),   TwoArg("LD", B, H),         TwoArg("LD", B, L),     TwoArg("LD", B, MHL),    TwoArg("LD", B, A),
    TwoArg("LD", C, B),       TwoArg("LD", C, C),      TwoArg("LD", C, D),       TwoArg("LD", C, E),   TwoArg("LD", C, H),         TwoArg("LD", C, L),     TwoArg("LD", C, MHL),    TwoArg("LD", C, A),
    TwoArg("LD", D, B),       TwoArg("LD", D, C),      TwoArg("LD", D, D),       TwoArg("LD", D, E),   TwoArg("LD", D, H),         TwoArg("LD", D, L),     TwoArg("LD", D, MHL),    TwoArg("LD", D, A),
    TwoArg("LD", E, B),       TwoArg("LD", E, C),      TwoArg("LD", E, D),       TwoArg("LD", E, E),   TwoArg("LD", E, H),         TwoArg("LD", E, L),     TwoArg("LD", E, MHL),    TwoArg("LD", E, A),
    TwoArg("LD", H, B),       TwoArg("LD", H, C),      TwoArg("LD", H, D),       TwoArg("LD", H, E),   TwoArg("LD", H, H),         TwoArg("LD", H, L),     TwoArg("LD", H, MHL),    TwoArg("LD", H, A),
    TwoArg("LD", L, B),       TwoArg("LD", L, C),      TwoArg("LD", L, D),       TwoArg("LD", L, E),   TwoArg("LD", L, H),         TwoArg("LD", L, L),     TwoArg("LD", L, MHL),    TwoArg("LD", L, A),
    TwoArg("LD", MHL, B),     TwoArg("LD", MHL, C),    TwoArg("LD", MHL, D),     TwoArg("LD", MHL, E), TwoArg("LD", MHL, H),       TwoArg("LD", MHL, L),   NoArg("HALT"),           TwoArg("LD", MHL, A),
    TwoArg("LD", A, B),       TwoArg("LD", A, C),      TwoArg("LD", A, D),       TwoArg("LD", A, E),   TwoArg("LD", A, H),         TwoArg("LD", A, L),     TwoArg("LD", A, MHL),    TwoArg("LD", A, A),
    TwoArg("ADD", A, B),      TwoArg("ADD", A, C),     TwoArg("ADD", A, D),      TwoArg("ADD", A, E),  TwoArg("ADD", A, H),        TwoArg("ADD", A, L),    TwoArg("ADD", A, MHL),   TwoArg("ADD", A, A),
    TwoArg("ADC", A, B),      TwoArg("ADC", A, C),     TwoArg("ADC", A, D),      TwoArg("ADC", A, E),  TwoArg("ADC", A, H),        TwoArg("ADC", A, L),    TwoArg("ADC", A, MHL),   TwoArg("ADC", A, A),
    OneArg("SUB",    B),      OneArg("SUB",    C),     OneArg("SUB",    D),      OneArg("SUB",    E),  OneArg("SUB",    H),        OneArg("SUB",    L),    OneArg("SUB",    MHL),   OneArg("SUB",    A),
    TwoArg("SBC", A, B),      TwoArg("SBC", A, C),     TwoArg("SBC", A, D),      TwoArg("SBC", A, E),  TwoArg("SBC", A, H),        TwoArg("SBC", A, L),    TwoArg("SBC", A, MHL),   TwoArg("SBC", A, A),
    OneArg("AND",    B),      OneArg("AND",    C),     OneArg("AND",    D),      OneArg("AND",    E),  OneArg("AND",    H),        OneArg("AND",    L),    OneArg("AND",    MHL),   OneArg("AND",    A),
    OneArg("XOR",    B),      OneArg("XOR",    C),     OneArg("XOR",    D),      OneArg("XOR",    E),  OneArg("XOR",    H),        OneArg("XOR",    L),    OneArg("XOR",    MHL),   OneArg("XOR",    A),
    OneArg("OR",     B),      OneArg("OR",     C),     OneArg("OR",     D),      OneArg("OR",     E),  OneArg("OR",     H),        OneArg("OR",     L),    OneArg("OR",     MHL),   OneArg("OR",     A),
    OneArg("CP",     B),      OneArg("CP",     C),     OneArg("CP",     D),      OneArg("CP",     E),  OneArg("CP",     H),        OneArg("CP",     L),    OneArg("CP",     MHL),   OneArg("CP",     A),
    OneArg("RET", NZ),        OneArg("POP", BC),       TwoArg("JP", NZ, Addr16), OneArg("JP", Addr16), TwoArg("CALL", NZ, Addr16), OneArg("PUSH", BC),     TwoArg("ADD", A, Imm8),  OneArg("RST", Fix("$00")),
    OneArg("RET", Z),         NoArg("RET"),            TwoArg("JP", Z, Addr16),  Prefix,               TwoArg("CALL", Z, Addr16),  OneArg("CALL", Addr16), TwoArg("ADC", A, Imm8),  OneArg("RST", Fix("$08")),
    OneArg("RET", NC),        OneArg("POP", DE),       TwoArg("JP", NC, Addr16), Invalid,              TwoArg("CALL", NC, Addr16), OneArg("PUSH", DE),     OneArg("SUB",    Imm8),  OneArg("RST", Fix("$10")),
    OneArg("RET", C),         NoArg("RETI"),           TwoArg("JP", C, Addr16),  Invalid,              TwoArg("CALL", C, Addr16),  Invalid,                TwoArg("SBC", A, Imm8),  OneArg("RST", Fix("$18")),
    TwoArg("LDH", Addr8, A),   OneArg("POP", HL),       TwoArg("LDH", MC, A),      Invalid,              Invalid,                    OneArg("PUSH", HL),     OneArg("AND",    Imm8),  OneArg("RST", Fix("$20")),
    TwoArg("ADD", SP, Rel8),  OneArg("JP", HL),        TwoArg("LD", Addr16, A),   Invalid,              Invalid,                    Invalid,                OneArg("XOR",    Imm8),  OneArg("RST", Fix("$28")),
    TwoArg("LDH", A, Addr8),  OneArg("POP", AF),       TwoArg("LDH", A, MC),     NoArg("DI"),          Invalid,                    OneArg("PUSH", AF),     OneArg("OR",     Imm8),  OneArg("RST", Fix("$30")),
    TwoArg("LDHL", SP, Rel8), TwoArg("LD", SP, HL),    TwoArg("LD", A, Addr16),  NoArg("EI"),          Invalid,                    Invalid,                OneArg("CP",     Imm8),  OneArg("RST", Fix("$38")),
];