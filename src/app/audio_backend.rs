use crate::emucore::audio::{AudioBackend, Frame, RINGBUF_DT, RINGBUF_RATE, RINGBUF_SIZE};
use cpal::{
    traits::{DeviceTrait, HostTrait, StreamTrait},
    OutputCallbackInfo, SampleFormat, SampleRate, Stream, StreamConfig, SupportedStreamConfigRange,
};
use ringbuf::Consumer;
use std::{
    fs::File,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering::Relaxed},
        Arc,
    },
    thread,
    time::Duration,
};
use wav;

pub struct NullBackend {
    thread: Option<thread::JoinHandle<()>>,
    stop_thread: Arc<AtomicBool>,
}

impl NullBackend {
    pub fn new() -> Self {
        NullBackend {
            thread: None,
            stop_thread: Arc::new(AtomicBool::new(false)),
        }
    }
}

impl AudioBackend for NullBackend {
    fn setup_stream(&mut self, mut queue: Consumer<Frame>) {
        let stop = self.stop_thread.clone();
        self.thread = Option::from(thread::spawn(move || {
            while !stop.load(Relaxed) {
                queue.discard(RINGBUF_SIZE);
                thread::sleep(Duration::from_millis(10));
            }
        }));
    }
}

impl Drop for NullBackend {
    fn drop(&mut self) {
        self.stop_thread.store(true, Relaxed);
        match self.thread.take() {
            Some(handle) => handle.join().unwrap(),
            _ => (),
        };
    }
}

pub struct WavFileBackend {
    thread: Option<thread::JoinHandle<std::io::Result<()>>>,
    stop_thread: Arc<AtomicBool>,
    output: &'static Path,
}

impl WavFileBackend {
    pub fn new(output: &'static Path) -> Self {
        WavFileBackend {
            thread: None,
            stop_thread: Arc::new(AtomicBool::new(false)),
            output,
        }
    }
}

impl AudioBackend for WavFileBackend {
    fn setup_stream(&mut self, mut queue: Consumer<Frame>) {
        let stop = self.stop_thread.clone();
        let output_path = self.output;
        self.thread = Option::from(thread::spawn(move || -> std::io::Result<()> {
            let mut data = Vec::new();
            while !stop.load(Relaxed) {
                while let Some(Frame(c1, c2)) = (&mut queue).next() {
                    data.push(c1);
                    data.push(c2);
                }
                thread::sleep(Duration::from_millis(10));
            }
            let header = wav::Header::new(wav::WAV_FORMAT_IEEE_FLOAT, 2, RINGBUF_RATE as u32, 32);
            let mut file = File::create(output_path)?;
            wav::write(header, &wav::BitDepth::ThirtyTwoFloat(data), &mut file)
        }));
    }
}

impl Drop for WavFileBackend {
    fn drop(&mut self) {
        self.stop_thread.store(true, Relaxed);
        match self.thread.take() {
            Some(handle) => handle.join().unwrap().unwrap(),
            _ => (),
        };
    }
}

struct CpalDriver {
    buffer: Consumer<Frame>,
    sample_rate: SampleRate,
    current: Frame,
    prev: Frame,
    interpolate: f32,
}

impl CpalDriver {
    fn new(buffer: Consumer<Frame>, sample_rate: SampleRate) -> Self {
        CpalDriver {
            buffer,
            sample_rate,
            current: Frame(0., 0.),
            prev: Frame(0., 0.),
            interpolate: 0.,
        }
    }

    fn advance_buffer(&mut self) {
        self.prev = self.current;
        self.current = match self.buffer.pop() {
            Some(f) => f,
            None => self.current * 0.999,
        }
    }

    fn next_frame(&mut self) -> Frame {
        let advance = 1. / self.sample_rate.0 as f32 / RINGBUF_DT;
        self.interpolate += advance;
        while self.interpolate >= 1. {
            self.advance_buffer();
            self.interpolate -= 1.;
        }
        self.current * self.interpolate + self.prev * (1. - self.interpolate)
    }
}

pub struct CpalBackend {
    stream: Option<Stream>,
}

impl CpalBackend {
    pub fn new() -> Self {
        CpalBackend { stream: None }
    }

    fn choose_stream_config(device: &impl DeviceTrait) -> StreamConfig {
        let configs = device
            .supported_output_configs()
            .expect("Could not retrieve stream configs");
        configs
            .filter(|c| c.channels() == 2 && c.sample_format() == SampleFormat::F32)
            .max_by(SupportedStreamConfigRange::cmp_default_heuristics)
            .expect("Could not find supported sound config")
            .with_max_sample_rate()
            .config()
    }
}

impl AudioBackend for CpalBackend {
    fn setup_stream(&mut self, queue: Consumer<Frame>) {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .expect("Failed to retrieve sound device");
        let config = Self::choose_stream_config(&device);
        let err_fn = |err| eprintln!("an error occurred on the output audio stream: {}", err);
        let mut driver = CpalDriver::new(queue, config.sample_rate);
        let output_fn = move |data: &mut [f32], _info: &OutputCallbackInfo| {
            for i in 0..data.len() / 2 {
                Frame(data[2 * i], data[2 * i + 1]) = driver.next_frame();
            }
        };
        let stream = device
            .build_output_stream(&config, output_fn, err_fn)
            .expect("Failed to create stream");
        stream.play().expect("Failed to start sound playback");
        self.stream = Some(stream);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::emucore::audio::SoundController;
    use std::sync::{Arc, Mutex};

    struct SoundGenerator {
        frequency: Arc<Mutex<u16>>,
        sample_num: u32,
        sample_rate: u32,
        previous_output: f32,
        previous_sample: f32,
        alpha: f32,
    }

    impl SoundGenerator {
        fn next_sample(&mut self) -> f32 {
            let t = self.sample_num as f32 / self.sample_rate as f32;
            let period = (2048 - *self.frequency.lock().unwrap()) as f32 / 131072.;
            let v = 0.2 * (((t % period < period / 2.) as i32) * 2 - 1) as f32;
            self.sample_num += 1;
            let out = self.alpha * (self.previous_sample + v - self.previous_output);
            self.previous_output = v;
            self.previous_sample = out;
            out
        }
    }

    #[test]
    fn test_cpal() {
        let host = cpal::default_host();
        let device = host.default_output_device().unwrap();
        let config = device.default_output_config().unwrap();
        let err_fn = |err| eprintln!("an error occurred on the output audio stream: {}", err);
        let freq = Arc::new(Mutex::new(0));
        let mut sound_generator = SoundGenerator {
            sample_num: 0,
            sample_rate: config.sample_rate().0,
            frequency: freq.clone(),
            previous_output: 0.,
            previous_sample: 0.,
            alpha: 0.99,
        };
        *freq.lock().unwrap() = 1750;
        let output_fn = move |data: &mut [f32], _info: &OutputCallbackInfo| {
            for i in 0..data.len() / 2 {
                let s = sound_generator.next_sample();
                data[2 * i] = s;
                data[2 * i + 1] = s;
            }
        };
        println!("{:?}", config.buffer_size());
        println!("{:?}", config.sample_rate());
        println!("{:?}", config.sample_format());
        let stream = device
            .build_output_stream(&config.into(), output_fn, err_fn)
            .expect("failed to build output stream");
        stream.play().unwrap();
        thread::sleep(Duration::from_secs(1));
        *freq.lock().unwrap() = 1899;
        thread::sleep(Duration::from_secs(1));
        stream.pause().unwrap();
    }

    #[test]
    fn test_file_backend() {
        let backend = Box::new(WavFileBackend::new(&Path::new("test_output/test_sound.wav")));
        let mut sc = SoundController::new(backend);
        sc.write(0xFF26, 0x80); // enable
        sc.write(0xFF24, 0x77); // L/R volume
        sc.write(0xFF25, 0x11); // L/R mix
        sc.write(0xFF11, 0x80); // C1 duty / length
        sc.write(0xFF12, 0xF4); // C1 volume / envelope
        sc.write(0xFF13, 0xC1); // C1 freq low
        sc.write(0xFF14, 0x87); // C1 restart / freq high

        for _ in 0..1 << 22 {
            sc.tick();
        }
    }
}
