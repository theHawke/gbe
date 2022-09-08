use std::path::Path;
use std::sync::{atomic::AtomicU8, mpsc, Arc, Mutex};

use gbe::app::audio_backend::CpalBackend;
use gbe::emucore::package::{EmulatorControl, Package};
use gbe::emucore::ppu::{FrameBuffer, LCD_HEIGHT, LCD_PIXELS, LCD_WIDTH};

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
        let control = Arc::new(Mutex::new(EmulatorControl::Run));
        let audio_backend = Box::new(CpalBackend::new());
        let buttons = Arc::new(AtomicU8::new(0xFF));
        let mut package = Package::new(
            Path::new("resources/roms/motocross.gb"),
            Some(Path::new("resources/bootrom/gb_bios.bin")),
            fb,
            audio_backend,
            buttons,
        )
        .unwrap();
        package.run(time_passed, control);
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
