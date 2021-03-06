use super::ppu::PpuCR;

pub trait MemoryIfc {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);

    fn get_cr(&self) -> &ControlRegisters;
    fn get_cr_mut(&mut self) -> &mut ControlRegisters;
}


pub struct GBMemory {
    wram: Box<[u8; 8192]>,
    vram: Box<[u8; 8192]>,
    oam: Box<[u8; 160]>,
    bootrom: Option<Box<[u8; 256]>>,
    cartridge: Cartridge,
    control_registers: ControlRegisters
}

impl GBMemory {
    pub fn new(cartridge: Cartridge) -> GBMemory {
        GBMemory {
            wram: Box::new([0; 8192]),
            vram: Box::new([0; 8192]),
            oam: Box::new([0; 160]),
            bootrom: None,
            cartridge,
            control_registers: ControlRegisters::new()
        }
    }
}

impl MemoryIfc for GBMemory {
    fn read(&self, addr: u16) -> u8 {
        if let Some(ref rom) = self.bootrom {
            if !self.control_registers.bootrom_disable && addr < 0x0100 {
                return rom[addr as usize]
            }
        }
        if addr < 0x8000 {
            self.cartridge.read_rom(addr)
        }
        else if addr < 0xA000 {
            self.vram[(addr & 0x1FFF) as usize]
        }
        else if addr < 0xC000 {
            self.cartridge.read_ram(addr & 0x1FFF)
        }
        else if addr < 0xFE00 {
            self.wram[(addr & 0x1FFF) as usize]
        }
        else if addr < 0xFEA0 {
            self.oam[(addr & 0x00FF) as usize]
        }
        else if addr < 0xFF00 {
            0 // empty memory region
        }
        else {
            self.control_registers.read_cr(addr)
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        if addr < 0x8000 {
            self.cartridge.write_rom(addr, val)
        }
        else if addr < 0xA000 {
            self.vram[(addr & 0x1FFF) as usize] = val
        }
        else if addr < 0xC000 {
            self.cartridge.write_ram(addr & 0x1FFF, val)
        }
        else if addr < 0xFE00 {
            self.wram[(addr & 0x1FFF) as usize] = val
        }
        else if addr < 0xFEA0 {
            self.oam[(addr & 0x00FF) as usize] = val
        }
        else if addr < 0xFF00 {
            // empty memory region
        }
        else {
            self.control_registers.write_cr(addr & 0x00FF, val)
        }
    }

    fn get_cr(&self) -> &ControlRegisters {
        &self.control_registers
    }

    fn get_cr_mut(&mut self) -> &mut ControlRegisters {
        &mut self.control_registers
    }
}

pub struct Cartridge {
    rom: Vec<u8>,
    ram: Vec<u8>,
}

impl Cartridge {
    pub fn new(rom: &[u8]) -> Cartridge {
        Cartridge {
            rom: Vec::from(rom),
            ram: Vec::new(),
        }
    }

    fn read_rom(&self, addr: u16) -> u8 {
        if (addr as usize) < self.rom.len() {
            self.rom[addr as usize]
        }
        else {
            0
        }
    }

    fn write_rom(&mut self, _addr: u16, _val: u8) {
        // memory bank controllers use rom writes for bank select
    }

    fn read_ram(&self, addr: u16) -> u8 {
        if (addr as usize) < self.ram.len() {
            self.ram[addr as usize]
        }
        else {
            0
        }
    }

    fn write_ram(&mut self, addr: u16, val: u8) {
        if (addr as usize) < self.ram.len() {
            self.ram[addr as usize] = val;
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ControlRegisters {
    extra_ram: Box<[u8; 127]>,
    pub interrupt_flag: u8,
    pub interrupt_enable: u8,
    bootrom_disable: bool,
    pub ppu_cr: PpuCR,
}

impl ControlRegisters {
    pub fn new() -> ControlRegisters {
        ControlRegisters {
            extra_ram: Box::new([0; 127]),
            interrupt_flag: 0xE0,
            interrupt_enable: 0,
            bootrom_disable: false,
            ppu_cr: PpuCR::new(),
        }
    }

    fn read_cr(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0xFEFF => panic!(),
            0xFF0F          => self.interrupt_flag,
            0xFF50          => self.bootrom_disable as u8,
            0xFF80..=0xFFFE => self.extra_ram[(addr & 0x007F) as usize],
            0xFFFF          => self.interrupt_enable,
            _               => 0xFF,
        }
    }

    fn write_cr(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0xFEFF => panic!(),
            0xFF0F          => self.interrupt_flag = val & 0x1F,
            0xFF50          => self.bootrom_disable |= val & 0x01 != 0,
            0xFF80..=0xFFFE => self.extra_ram[(addr & 0x007F) as usize] = val,
            0xFFFF          => self.interrupt_enable = val & 0x1F,
            _ => {},
        }
    }
}