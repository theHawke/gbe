use super::cpu::{InterruptType, Interruptible};
use super::mem::MemoryIfc;

use std::cell::{Ref, RefCell, RefMut};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

const MAX_SPRITES_PER_LINE: usize = 10;
pub const LCD_WIDTH: usize = 160;
pub const LCD_HEIGHT: usize = 144;
pub const LCD_PIXELS: usize = LCD_WIDTH * LCD_HEIGHT;
const VBLANK_LINES: usize = 10;
const LINES_PER_FRAME: usize = LCD_HEIGHT + VBLANK_LINES;
const _DOTS_PER_FRAME: usize = 70224;
const DOTS_PER_LINE: usize = 456;
const MODE_2_DOTS: usize = 80;
const MODE_3_DOTS_MIN: usize = 172;
const MODE_3_DOTS_PER_SPRITE: usize = 12;

pub type FrameBuffer = [u8; LCD_PIXELS];

pub struct Ppu<M: MemoryIfc, C: Interruptible> {
    mem: Rc<RefCell<M>>,
    cpu: Rc<RefCell<C>>,
    line_sprites: [u8; MAX_SPRITES_PER_LINE],
    num_line_sprites: usize,
    fb: Arc<Mutex<FrameBuffer>>,
    current_line: u8,
    current_dot_on_line: u16,
    stat_interrupt_prev: bool,
    line_buf_bg_win: [u8; LCD_WIDTH],
    line_buf_obj: [u8; LCD_WIDTH],
    line_buf_obj_x: [bool; LCD_WIDTH],
    line_buf_obj_prio: [bool; LCD_WIDTH],
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct PpuCR {
    pub lcdc: u8,
    pub stat: u8,
    pub scy: u8,
    pub scx: u8,
    pub ly: u8,
    pub lyc: u8,
    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,
    pub wy: u8,
    pub wx: u8,
}

impl PpuCR {
    pub fn new() -> PpuCR {
        PpuCR {
            lcdc: 0,
            stat: 0,
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            bgp: 0xE4,
            obp0: 0xE4,
            obp1: 0xE4,
            wy: 0,
            wx: 0,
        }
    }

    fn display_on(&self) -> bool {
        self.lcdc & 0x80 != 0
    }

    fn window_on(&self) -> bool {
        self.lcdc & 0x20 != 0
    }

    fn bg_window_on(&self) -> bool {
        self.lcdc & 0x01 != 0
    }

    fn sprites_on(&self) -> bool {
        self.lcdc & 0x02 != 0
    }

    fn big_sprites(&self) -> bool {
        self.lcdc & 0x04 != 0
    }

    fn bg_win_tile_addr(&self, id: u8) -> u16 {
        if self.lcdc & 0x10 != 0 {
            0x8000 + (id as u16) * 16
        } else {
            0x9000u16.wrapping_add((id as i8 as u16) * 16)
        }
    }

    fn bg_map_addr(&self) -> u16 {
        if self.lcdc & 0x08 != 0 {
            0x9C00
        } else {
            0x9800
        }
    }

    fn window_map_addr(&self) -> u16 {
        if self.lcdc & 0x40 != 0 {
            0x9C00
        } else {
            0x9800
        }
    }
}

struct OamEntry {
    y: u8,
    x: u8,
    tile: u8,
    flags: u8,
}

impl OamEntry {
    fn flip_x(&self) -> bool {
        self.flags & 0x20 != 0
    }

    fn flip_y(&self) -> bool {
        self.flags & 0x40 != 0
    }

    fn low_priority(&self) -> bool {
        self.flags & 0x80 != 0
    }

    fn pallette_0(&self) -> bool {
        self.flags & 0x10 == 0
    }
}

impl<M: MemoryIfc, C: Interruptible> Ppu<M, C> {
    pub fn new(mem: Rc<RefCell<M>>, cpu: Rc<RefCell<C>>) -> Ppu<M, C> {
        Ppu {
            mem,
            cpu,
            line_sprites: [u8::MAX; MAX_SPRITES_PER_LINE],
            num_line_sprites: 0,
            fb: Arc::new(Mutex::new([0; LCD_PIXELS])),
            current_line: 0,
            current_dot_on_line: 0,
            stat_interrupt_prev: false,
            line_buf_bg_win: [0; LCD_WIDTH],
            line_buf_obj: [0; LCD_WIDTH],
            line_buf_obj_x: [false; LCD_WIDTH],
            line_buf_obj_prio: [false; LCD_WIDTH],
        }
    }

    pub fn with_external_fb(mem: Rc<RefCell<M>>, cpu: Rc<RefCell<C>>, fb: Arc<Mutex<FrameBuffer>>) -> Ppu<M, C> {
        Ppu {
            mem,
            cpu,
            line_sprites: [u8::MAX; MAX_SPRITES_PER_LINE],
            num_line_sprites: 0,
            fb,
            current_line: 0,
            current_dot_on_line: 0,
            stat_interrupt_prev: false,
            line_buf_bg_win: [0; LCD_WIDTH],
            line_buf_obj: [0; LCD_WIDTH],
            line_buf_obj_x: [false; LCD_WIDTH],
            line_buf_obj_prio: [false; LCD_WIDTH],
        }
    }

    fn cr(&self) -> Ref<PpuCR> {
        Ref::map(self.mem.borrow(), |mem| &mem.get_cr().ppu_cr)
    }

    fn cr_mut(&mut self) -> RefMut<PpuCR> {
        RefMut::map(self.mem.borrow_mut(), |mem| &mut mem.get_cr_mut().ppu_cr)
    }

    // rendering related functions
    fn read_oam(mem: &M, i: usize) -> OamEntry {
        let entry = 0xFE00 + 4 * (i as u16);
        OamEntry {
            y: mem.read(entry),
            x: mem.read(entry + 1),
            tile: mem.read(entry + 2),
            flags: mem.read(entry + 3),
        }
    }

    fn tile_bytes(mem: &M, tile_addr: u16, tile_y: u8) -> (u8, u8) {
        let low_byte = mem.read(tile_addr + 2 * tile_y as u16);
        let high_byte = mem.read(tile_addr + 2 * tile_y as u16 + 1);
        (low_byte, high_byte)
    }

    fn map_tile(mem: &M, cr: &PpuCR, map_y: u8, map_x: u8, map_base: u16) -> (u8, u8) {
        let tile_idx = (map_y as u16 & 0xF8) << 2 | map_x as u16 >> 3;
        let tile = mem.read(map_base + tile_idx);
        let tile_addr = cr.bg_win_tile_addr(tile);
        let tile_y = map_y & 0x07;
        Self::tile_bytes(mem, tile_addr, tile_y)
    }

    fn get_color(pallette: u8, idx: u8) -> u8 {
        pallette >> 2 * idx & 0x03
    }

    fn search_sprites(&mut self, y: u8) {
        let cr = *self.cr();
        if cr.sprites_on() {
            let mem_r = self.mem.borrow();
            let mem = mem_r.deref();
            let y_lower = y + 16;
            let y_upper = if cr.big_sprites() { y + 32 } else { y + 24 };
            for oam_idx in 0..40 {
                let entry = Self::read_oam(mem, oam_idx as usize);
                let ey = entry.y;
                let ex = entry.x;
                if ey >= y_lower && ey < y_upper {
                    // insertion sort into the sprites array
                    // sort by priority, i.e. lower x-values and then lower oam-index
                    // only the 10 highest priority sprites are kept
                    let mut insert_cur = oam_idx;
                    for cursor in 0..self.num_line_sprites {
                        if ex < Self::read_oam(mem, self.line_sprites[cursor] as usize).x {
                            std::mem::swap(&mut insert_cur, &mut self.line_sprites[cursor]);
                        }
                    }
                    if self.num_line_sprites < MAX_SPRITES_PER_LINE {
                        self.line_sprites[self.num_line_sprites] = insert_cur;
                        self.num_line_sprites += 1;
                    }
                }
            }
        }
    }

    fn render_line(&mut self, y: u8) {
        let cr = *self.cr();
        let mem_r = self.mem.borrow();
        let mem = mem_r.deref();
        // display off => blank line/screen
        if !cr.display_on() {
            let mut fb = self.fb.lock().unwrap();
            for i in (LCD_WIDTH * y as usize)..(LCD_WIDTH * (y + 1) as usize) {
                fb[i] = 0;
            }
            return;
        }

        if !cr.bg_window_on() {
            for x in 0..LCD_WIDTH {
                self.line_buf_bg_win[x] = 0;
            }
        } else {
            let window_start = if cr.window_on() && y >= cr.wy {
                cr.wx as i32 - 7
            } else {
                LCD_WIDTH as i32
            };

            // fetch first tile
            let mut map_y = cr.scy + y;
            let mut map_x = cr.scx;
            let mut map_base = cr.bg_map_addr();
            if window_start < 0 {
                map_y = y - cr.wy;
                map_x = -window_start as u8;
                map_base = cr.window_map_addr();
            }
            let (mut low_byte, mut high_byte) = Self::map_tile(mem, &cr, map_y, map_x, map_base);
            let mut bit_x = 7 - (map_x & 0x07);
            for x in 0..LCD_WIDTH as i32 {
                // transition to window
                if x == window_start {
                    map_y = y - cr.wy;
                    map_x = (x - window_start) as u8;
                    map_base = cr.window_map_addr();
                    (low_byte, high_byte) = Self::map_tile(mem, &cr, map_y, map_x, map_base);
                    bit_x = 7 - (map_x & 0x07);
                }

                // draw bg tile
                let pixel = low_byte >> bit_x & 0x01 | (high_byte >> bit_x & 0x01) << 1;
                self.line_buf_bg_win[x as usize] = pixel;

                // possibly fetch new bg tile
                if bit_x == 0 {
                    map_x += 8;
                    (low_byte, high_byte) = Self::map_tile(mem, &cr, map_y, map_x, map_base);
                }

                bit_x = bit_x.wrapping_sub(1) & 0x07;
            }
        }

        if cr.sprites_on() {
            self.line_buf_obj.fill(0);
            self.line_buf_obj_x.fill(false);
            self.line_buf_obj_prio.fill(false);
            for obj_idx in (0..self.num_line_sprites).rev() {
                let obj_i = self.line_sprites[obj_idx];
                let obj = Self::read_oam(mem, obj_i as usize);
                let obj_y = y + 16 - obj.y;
                let tile = if cr.big_sprites() {
                    if (obj_y < 8) ^ obj.flip_y() {
                        obj.tile & 0xFE
                    } else {
                        obj.tile | 0x01
                    }
                } else {
                    obj.tile
                };
                let tile_addr = 0x8000 + 16 * tile as u16;
                let tile_y = if obj.flip_y() {
                    7 - (obj_y & 0x07)
                } else {
                    obj_y & 0x07
                };
                let (low_byte_, high_byte_) = Self::tile_bytes(mem.deref(), tile_addr, tile_y);
                let (low_byte, high_byte) = if obj.flip_x() {
                    (low_byte_.reverse_bits(), high_byte_.reverse_bits())
                } else {
                    (low_byte_, high_byte_)
                };
                let x_left: i32 = obj.x as i32 - 8;
                let (x_start, mut bit_x) = if x_left >= 0 {
                    (x_left as u8, 8)
                } else {
                    (0, (8 + x_left) as u8)
                };
                let x_end = std::cmp::min(obj.x, LCD_WIDTH as u8);
                for x in x_start..x_end {
                    bit_x -= 1;
                    let pixel = low_byte >> bit_x & 0x01 | (high_byte >> bit_x & 0x01) << 1;
                    if pixel != 0 {
                        // 0 => transparent
                        let pallette = if obj.pallette_0() { cr.obp0 } else { cr.obp1 };
                        self.line_buf_obj[x as usize] = Self::get_color(pallette, pixel);
                        self.line_buf_obj_x[x as usize] = true;
                        self.line_buf_obj_prio[x as usize] = obj.low_priority();
                    }
                }
            }
        }

        // merge bg/win and obj layer
        let fb_line_start = LCD_WIDTH * y as usize;
        let mut fb = self.fb.lock().unwrap();
        for x in 0..LCD_WIDTH {
            fb[fb_line_start + x] = if cr.sprites_on()
                && self.line_buf_obj_x[x]
                && (!self.line_buf_obj_prio[0] || self.line_buf_bg_win[x] == 0)
            {
                self.line_buf_obj[x]
            } else {
                Self::get_color(cr.bgp, self.line_buf_bg_win[x])
            };
        }
    }

    #[cfg(test)]
    fn render_frame(&mut self) {
        for y in 0..LCD_HEIGHT as u8 {
            self.search_sprites(y);
            self.render_line(y);
        }
    }

    fn check_stat_interrupt(&self) -> bool {
        let stat = self.cr().stat;
        let mode = stat & 0x03;
        stat & 0x40 != 0 && stat & 0x04 != 0 || mode != 3 && stat & 0x08 << mode != 0
    }

    pub fn tick(&mut self) -> bool {
        // stat updates
        if self.current_dot_on_line == 0 {
            self.cr_mut().ly = self.current_line;
            let stat_flags = self.cr().stat & 0x78;
            let lyc_match = self.current_line == self.cr().lyc;
            self.cr_mut().stat = stat_flags | (lyc_match as u8) << 2;
            match self.current_line {
                0..=143 => {
                    self.cr_mut().stat |= 0x02; // Mode 2: OAM access, mode-bits = 10
                }
                144 => {
                    self.cr_mut().stat |= 0x01; // Mode 1: VBlank, mode-bits = 01
                    self.cpu.borrow_mut().raise_interrupt(InterruptType::VBlank);
                }
                145..=153 => {}
                _ => panic!(),
            }
        }

        // drawing
        if self.current_line < LCD_HEIGHT as u8 {
            if self.current_dot_on_line == 0 {
                self.search_sprites(self.current_line);
            }
            if self.current_dot_on_line == MODE_2_DOTS as u16 {
                self.cr_mut().stat |= 0x03; // mode-bits = 11
                self.render_line(self.current_line);
            } else if self.current_dot_on_line
                == (MODE_3_DOTS_MIN + self.num_line_sprites * MODE_3_DOTS_PER_SPRITE) as u16
            {
                self.cr_mut().stat &= !0x03; // mode-bits = 00
            }
        }

        // update stat interrupt
        let stat_interrupt_new = self.check_stat_interrupt();
        if !self.stat_interrupt_prev && stat_interrupt_new {
            self.cpu
                .borrow_mut()
                .raise_interrupt(InterruptType::LcdStat);
        }
        self.stat_interrupt_prev = stat_interrupt_new;

        // advance dot
        self.current_dot_on_line += 1;
        if self.current_dot_on_line == DOTS_PER_LINE as u16 {
            self.current_dot_on_line = 0;
            self.current_line += 1;
            if self.current_line == LINES_PER_FRAME as u8 {
                self.current_line = 0;
                return true;
            }
        }
        false
    }

    pub fn get_frame<'a>(&'a self) -> impl Deref<Target = FrameBuffer> + 'a {
        self.fb.lock().unwrap()
    }

    pub fn get_fb(&self) -> Arc<Mutex<FrameBuffer>> {
        self.fb.clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::emucore::mem::ControlRegisters;
    use image::{save_buffer, ColorType, ImageError};
    use std::collections::HashMap;

    struct Dummy {
        cr: ControlRegisters,
        mem: HashMap<u16, u8>,
    }

    impl<I> From<I> for Dummy
    where
        I: IntoIterator<Item = (u16, u8)>,
    {
        fn from(iter: I) -> Dummy {
            Dummy {
                mem: HashMap::from_iter(iter),
                cr: ControlRegisters::new(),
            }
        }
    }

    impl MemoryIfc for Dummy {
        fn get_cr(&self) -> &ControlRegisters {
            &self.cr
        }
        fn get_cr_mut(&mut self) -> &mut ControlRegisters {
            &mut self.cr
        }
        fn read(&self, addr: u16) -> u8 {
            self.mem.get(&addr).map_or(0, |x| *x)
        }
        fn write(&mut self, addr: u16, val: u8) {
            self.mem.insert(addr, val);
        }
    }

    impl Interruptible for Dummy {
        fn raise_interrupt(&mut self, _inter_type: InterruptType) {}
    }

    #[test]
    fn test_draw() -> Result<(), ImageError> {
        let mem = Rc::new(RefCell::new(Dummy::from([
            (0x8010, 0xAA),
            (0x8011, 0xF0),
            (0x9800, 0x01),
        ])));
        mem.borrow_mut().cr.ppu_cr.lcdc = 0x91;
        let mut ppu = Ppu::new(mem.clone(), mem);
        ppu.render_frame();
        let mut image = [0u8; LCD_PIXELS];
        let fb = ppu.fb.lock().unwrap();
        for i in 0..image.len() {
            let v = fb[i];
            image[i] = v * v * 27;
        }
        save_buffer(
            "test_output/test_draw.png",
            &image,
            LCD_WIDTH as u32,
            LCD_HEIGHT as u32,
            ColorType::L8,
        )
    }
}
