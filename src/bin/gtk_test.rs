use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex, mpsc};

use gbe::app::audio_backend::CpalBackend;
use gbe::emucore::audio::SoundController;
use gbe::emucore::cpu::Cpu;
use gbe::emucore::mem::{Cartridge, GBMemory};
use gbe::emucore::ppu::{LCD_HEIGHT, LCD_PIXELS, LCD_WIDTH, Ppu, FrameBuffer};
use gtk::cairo::{Filter, FontFace, FontSlant, FontWeight, Format, ImageSurface};
use gtk::{prelude::*, Application, ApplicationWindow, DrawingArea};

const APP_ID: &str = "de.theHawke.gbe";

const PALLETTE: [u32; 4] = [0xFFFFFFFF, 0xFF808080, 0xFF404040, 0xFF000000];

fn main() {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_ui);

    // Run the application
    app.run();
}

fn start_emulator(fb: Arc<Mutex<FrameBuffer>>, time_passed: mpsc::Receiver<i64>) {
    std::thread::spawn(move || {
        let audio_backend = Box::new(CpalBackend::new());
        let sound_controller = Rc::new(RefCell::new(SoundController::new(audio_backend)));
        let bootrom = include_bytes!("../../resources/bootrom/gb_bios.bin");
        let mut cartridge_bytes = [0u8; 334];
        cartridge_bytes[256] = 0x18;
        cartridge_bytes[257] = 0xFE;
        for i in 0..0x30 {
            cartridge_bytes[0x104 + i] = bootrom[0xA8 + i];
        }
        cartridge_bytes[333] = !(0x19 - 1);
        let cartridge = Cartridge::new(&cartridge_bytes);
        let memory = Rc::new(RefCell::new(GBMemory::with_bootrom(bootrom, cartridge, sound_controller.clone())));
        let cpu = Rc::new(RefCell::new(Cpu::new(memory.clone())));
        let mut ppu = Ppu::with_external_fb(memory.clone(), cpu.clone(), fb);
        
        let mut time = time_passed.recv().unwrap();
        let mut next_time = time;
        'main: while cpu.borrow().running() {
            let frame = ppu.tick();
            sound_controller.borrow_mut().tick();
            cpu.borrow_mut().tick();
            if frame {
                // notify main thread to redraw

                // wait for notification to run next frame
                while next_time < time {
                    next_time = match time_passed.recv() {
                        Ok(t) => t,
                        Err(_) => {
                            eprintln!("timing sender shut down => stopping emulator");
                            break 'main
                        }
                    }
                }
                time += 16742; // (59.73 Hz)^-1 in µs
            }
        }
    });
}

fn build_ui(app: &Application) {
    let fb = Arc::new(Mutex::new([0u8; LCD_PIXELS]));
    let fb_render = fb.clone();
    let mut surface =
        ImageSurface::create(Format::ARgb32, LCD_WIDTH as i32, LCD_HEIGHT as i32).unwrap();

    let screen = DrawingArea::builder()
        .height_request(LCD_HEIGHT as i32)
        .width_request(LCD_WIDTH as i32)
        .build();

    screen.set_draw_func(move |da, cr, w, h| {
        // clear screen
        cr.set_source_rgba(0., 0., 0., 1.);
        cr.paint().unwrap();

        // gb frame buffer to surface
        {
            let mut buf_u8 = surface.data().unwrap();
            let buf = unsafe {
                let (pre, main, post) = buf_u8.align_to_mut::<u32>();
                assert!(pre.len() == 0);
                assert!(post.len() == 0);
                assert!(main.len() == LCD_PIXELS);
                main
            };
            let fb = fb_render.lock().unwrap();
            for offs in 0..LCD_PIXELS {
                buf[offs] = PALLETTE[fb[offs] as usize];
            }
        }
        
        // scaled surface to screen
        let s = std::cmp::min(w / LCD_WIDTH as i32, h / LCD_HEIGHT as i32);
        cr.scale(s as f64, s as f64);
        let x_offs = ((w - s * LCD_WIDTH as i32) / 2) as f64;
        let y_offs = ((h - s * LCD_HEIGHT as i32) / 2) as f64;
        cr.set_source_surface(&surface, x_offs / s as f64, y_offs / s as f64)
            .unwrap();
        cr.source().set_filter(Filter::Nearest);
        cr.paint().unwrap();

        // fps counter
        cr.identity_matrix();
        cr.set_source_rgb(0., 1., 0.);
        cr.move_to(10., 30.);
        cr.set_font_size(20.);
        let face = FontFace::toy_create(
            &cr.font_face().toy_get_family().unwrap(),
            FontSlant::Normal,
            FontWeight::Bold,
        )
        .unwrap();
        cr.set_font_face(&face);
        let fps_text = format!("{:.2}", da.frame_clock().unwrap().fps());
        cr.show_text(&fps_text).unwrap();
    });

    let (tx, rx) = mpsc::channel();

    screen.add_tick_callback(move |da, fc| {
        da.queue_draw();
        match tx.send(fc.frame_time()) {
            Ok(()) => Continue(true),
            Err(_) => {
                eprintln!("frame time send failed, emulator down?");
                Continue(false)
            }
        }
    });

    start_emulator(fb, rx);

    // Create a window
    let window = ApplicationWindow::builder()
        .application(app)
        .title("GameBoy Emulator")
        .child(&screen)
        .build();

    // Present window
    window.present();
}
