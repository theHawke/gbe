pub struct MemoryController<'a> {
    wram: Box<[u8; 8192]>,
    vram: Box<[u8; 8192]>,
    oam: Box<[u8; 160]>,
    cartridge: Box<Cartridge<'a>>,
    control_registers: ControlRegisters
}

impl<'a> MemoryController<'a> {
    pub fn new(cartridge: Box<Cartridge>) -> MemoryController {
        MemoryController {
            wram: Box::new([0; 8192]),
            vram: Box::new([0; 8192]),
            oam: Box::new([0; 160]),
            cartridge,
            control_registers: ControlRegisters::new()
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
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
            self.control_registers.read_cr(addr & 0x00FF)
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
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
}

pub struct Cartridge<'a> {
    rom: &'a [u8],
    ram: Vec<u8>,
}

impl<'a> Cartridge<'a> {
    pub fn new(rom: &[u8]) -> Cartridge {
        Cartridge {
            rom,
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

    fn write_rom(&mut self, addr: u16, val: u8) {
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

pub struct ControlRegisters {
    extra_ram: Box<[u8; 127]>,
    cpu_cr: CpuControlRegisters,
    ppu_cr: PpuControlRegisters,
    audio_cr: AudioControlRegisters
}

impl ControlRegisters {
    pub fn new() -> ControlRegisters {
        ControlRegisters {
            extra_ram: Box::new([0; 127]),
            cpu_cr: CpuControlRegisters {  },
            ppu_cr: PpuControlRegisters {  },
            audio_cr: AudioControlRegisters {  },
        }
    }

    fn read_cr(&self, addr: u16) -> u8 {
        0
    }

    fn write_cr(&mut self, addr: u16, val: u8) {

    }
}

pub struct CpuControlRegisters {

}

pub struct PpuControlRegisters {

}

pub struct AudioControlRegisters {

}