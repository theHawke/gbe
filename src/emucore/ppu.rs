use super::mem::MemoryIfc;
use super::cpu::{Interruptible, InterruptType};

use std::rc::Rc;
use std::cell::RefCell;


pub struct Ppu<M: MemoryIfc, C: Interruptible> {
    mem: Rc<RefCell<M>>,
    cpu: Rc<RefCell<C>>,
    cr: PpuCR,
    fb: Box<[u8; 160*144]>,
}

pub struct PpuCR {
    lcdc: u8,
    stat: u8,
    scy: u8,
    scx: u8,
    ly: u8,
    lyc: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    wy: u8,
    wx: u8,
}

impl PpuCR {
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
        }
        else {
            0x9000u16.wrapping_add((id as i8 as u16) * 16)
        }
    }

    fn bg_map_addr(&self) -> u16 {
        if self.lcdc & 0x08 != 0 {
            0x9C00
        }
        else {
            0x9800
        }
    }

    fn window_map_addr(&self) -> u16 {
        if self.lcdc & 0x40 != 0 {
            0x9C00
        }
        else {
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

impl<M: MemoryIfc, C: Interruptible> Ppu<M, C>{

    fn read_oam(&self, i: usize) -> OamEntry {
        let mem = self.mem.borrow();
        let entry = 0xFE00 + 4*(i as u16);
        OamEntry {
            y: mem.read(entry),
            x: mem.read(entry + 1),
            tile: mem.read(entry + 2),
            flags: mem.read(entry + 3),
        }
    }

    fn get_color(pallette: u8, idx: u8) -> u8 {
        pallette >> 2*idx & 0x03
    }

    fn get_from_tile(&self, tile_addr: u16, tile_x: u8, tile_y: u8) -> u8 {
        let mem = self.mem.borrow();
        let tile_bit_0 = mem.read(tile_addr + 2 * tile_y as u16);
        let tile_bit_1 = mem.read(tile_addr + 2 * tile_y as u16 + 1);
        tile_bit_0 >> (7 - tile_x) & 0x01 | (tile_bit_1 >> (7 - tile_x) & 0x01) << 1
    }

    pub fn render_line(&mut self, y: u8) {
        // preprocess sprites
        const MAX_SPRITES_PER_LINE: usize = 10;
        let mut sprites = [u8::MAX; MAX_SPRITES_PER_LINE];
        let mut num_sprites = 0;
        let y_lower = y + 16;
        let y_upper = if self.cr.big_sprites() { y + 32 } else { y + 24 };
        for oam_idx in 0..40 {
            let entry = self.read_oam(oam_idx as usize);
            let ey = entry.y;
            let ex = entry.x;
            if ey >= y_lower && ey < y_upper {
                // insertion sort into the sprites array
                // sort by priority, i.e. lower x-values and then lower oam-index
                // only the 10 highest priority sprites are kept
                let mut insert_cur = oam_idx;
                for cursor in 0..num_sprites {
                    if ex < self.read_oam(sprites[cursor] as usize).x {
                        std::mem::swap(&mut insert_cur, &mut sprites[cursor]);
                    }
                }
                if num_sprites < MAX_SPRITES_PER_LINE {
                    sprites[num_sprites] = insert_cur;
                    num_sprites += 1;
                }
            }
        }

        // render line
        for x in 0..160 {
            let mut color_0 = true;
            let fb_index = x as usize + 160 * y as usize;
            if self.cr.bg_window_on() {
                let (map_x, map_y, map_base) = if self.cr.window_on() && x + 7 >= self.cr.wx && y >= self.cr.wy {
                    // Window
                    (x + 7 - self.cr.wx, y - self.cr.wy, self.cr.window_map_addr())
                } else {
                    // Background
                    (x.wrapping_add(self.cr.scx), y.wrapping_add(self.cr.scy), self.cr.bg_map_addr())
                };
                let tile_idx = (map_x as u16 >> 3) + (map_y as u16 >> 3) << 5;
                let tile = self.mem.borrow().read(map_base + tile_idx);
                let color_idx = self.get_from_tile(self.cr.bg_win_tile_addr(tile), map_x & 0x07, map_y & 0x07);
                self.fb[fb_index] = Self::get_color(self.cr.bgp, color_idx);
                color_0 = color_idx == 0;
            }
            if self.cr.sprites_on() {
                for i in 0..MAX_SPRITES_PER_LINE {
                    let obj = self.read_oam(i);
                    if x + 8 < obj.x { break; }
                    if x >= obj.x { continue; }
                    // draw this sprite at this position
                    let obj_x = x + 8 - obj.x;
                    let obj_y = y + 16 - obj.y;
                    let tile = if self.cr.big_sprites() {
                        if obj_y < 8 { obj.tile & 0xFE } else { obj.tile | 0x01 }
                    } else {
                        obj.tile
                    };
                    let color_idx = self.get_from_tile(
                        0x8000 + 16 * tile as u16, 
                        if obj.flip_x() { 7 - obj_x } else { obj_x }, 
                        if obj.flip_y() { 7 - (obj_y & 0x07)} else { obj_y & 0x07 }
                    );
                    if color_idx == 0 {
                        // sprite is transparent at this pixel => look for another sprite here
                        continue;
                    }
                    if !obj.low_priority() || color_0 {
                        self.fb[fb_index] = Self::get_color(if obj.pallette_0() { self.cr.obp0 } else { self.cr.obp1 }, color_idx);
                    }
                    break;
                }
            }
        }
    }

    pub fn render_frame(&mut self) {
        for y in 0..144 {
            self.render_line(y);
        }
    }

}