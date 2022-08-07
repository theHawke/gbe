use gbe::{app::audio_backend::WavFileBackend, emucore::{
    audio::SoundController,
    mem::{Cartridge, GBMemory},
    ppu::{Ppu, LCD_PIXELS, LCD_WIDTH, LCD_HEIGHT},
    cpu::Cpu,
}, util::disas::{disassemble_one, at_addr}};
use image::save_buffer;
use std::path::Path;
use std::cell::RefCell;
use std::rc::Rc;
use std::fs::{create_dir, remove_dir_all};

const TEST_PALLETTE: [[u8; 3]; 4] = [[255, 255, 255], [150, 150, 150], [50, 50 , 50], [0, 0, 0]];

fn render(fb: &[u8], pallette: [[u8; 3]; 4]) -> Box<[u8; 3*LCD_PIXELS]> {
    let mut output = Box::new([0u8; 3*LCD_PIXELS]);
    for i in 0..LCD_PIXELS {
        let pixel = pallette[fb[i] as usize];
        output[3*i] = pixel[0];
        output[3*i + 1] = pixel[1];
        output[3*i + 2] = pixel[2];
    }
    output
}

#[test]
fn test_bootrom() -> Result<(), image::ImageError> {
    let output_path = Path::new("test_output/bootrom_test/");
    let _ = remove_dir_all(output_path);
    let _ = create_dir(output_path);

    let audio_backend = Box::new(WavFileBackend::new(Path::new("test_output/bootrom_test/sound.wav")));
    let sound_controller = Rc::new(RefCell::new(SoundController::new(audio_backend)));
    let bootrom = include_bytes!("../resources/bootrom/gb_bios.bin");
    let mut cartridge_bytes = [0u8; 334];
    cartridge_bytes[256] = 0x76;
    for i in 0..0x30 {
        cartridge_bytes[0x104 + i] = bootrom[0xA8 + i];
    }
    cartridge_bytes[333] = !(0x19 - 1);
    let cartridge = Cartridge::new(&cartridge_bytes);
    let memory = Rc::new(RefCell::new(GBMemory::with_bootrom(bootrom, cartridge, sound_controller.clone())));
    let cpu = Rc::new(RefCell::new(Cpu::new(memory.clone())));
    let mut ppu = Ppu::new(memory.clone(), cpu.clone());
    
    let mut frame_count = 0;
    let mut _instr_count = 0;
    let print_instr = false;

    while cpu.borrow().running() {
        let frame = ppu.tick();
        sound_controller.borrow_mut().tick();
        cpu.borrow_mut().tick();
        if frame {
            let file_name = format!("test_output/bootrom_test/frame{}.png", frame_count);
            let output_file = Path::new(file_name.as_str());
            let data = render(ppu.get_frame(), TEST_PALLETTE);
            save_buffer(output_file, data.as_ref(), LCD_WIDTH as u32, LCD_HEIGHT as u32, image::ColorType::Rgb8)?;
            frame_count += 1;
        }
        if cpu.borrow().new_instr() {
            let addr = cpu.borrow().get_pc().wrapping_sub(1);
            if print_instr {
                println!("{:04x}  {}", addr, disassemble_one(&mut at_addr(&*memory.borrow(), addr)).unwrap());
            }
            _instr_count += 1;
        }
    }
    Ok(())
}