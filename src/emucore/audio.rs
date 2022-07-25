use std::sync::{Arc, Mutex};

pub struct SoundController {
    ticks: u64,
    c1: Channel1,
    c2: Channel2,
    c3: Channel3,
    c4: Channel4,
    master_enable: bool,
    mixer1: [bool; 4],
    mixer2: [bool; 4],
    volume1: u8,
    volume2: u8,
    prev1: f32,
    prev2: f32,
    prev_out1: f32,
    prev_out2: f32,
    alpha: f32,
}

impl SoundController {
    pub fn new() -> Self {
        SoundController {
            ticks: 0,
            c1: Channel1::new(),
            c2: Channel2::new(),
            c3: Channel3::new(),
            c4: Channel4::new(),
            master_enable: false,
            mixer1: [false; 4],
            mixer2: [false; 4],
            volume1: 0,
            volume2: 0,
            prev1: 0.,
            prev2: 0.,
            prev_out1: 0.,
            prev_out2: 0.,
            alpha: 0.999,
        }
    }

    pub fn tick(&mut self) {
        // push samples into the ringbuffer at 65536 Hz
        const SAMPLING_SHIFT: i32 = 6;
        const SAMPLING_MASK: u64 = (1 << SAMPLING_SHIFT) - 1;
        self.ticks += 1;
        if self.ticks & 0x07 == 0 {
            self.c4.tick();
        }
        if self.ticks & 0x03 == 0 {
            self.c1.tick();
            self.c2.tick();
        }
        if self.ticks & 0x01 == 0 {
            self.c3.tick();
        }
        if self.ticks & SAMPLING_MASK == 0 {
            let mut preamp1 = 0.;
            let mut preamp2 = 0.;
            if self.mixer1[0] {
                preamp1 += self.c1.get_output();
            }
            if self.mixer1[1] {
                preamp1 += self.c2.get_output();
            }
            if self.mixer1[2] {
                preamp1 += self.c3.get_output();
            }
            if self.mixer1[3] {
                preamp1 += self.c4.get_output();
            }
            if self.mixer2[0] {
                preamp2 += self.c1.get_output();
            }
            if self.mixer2[1] {
                preamp2 += self.c2.get_output();
            }
            if self.mixer2[2] {
                preamp2 += self.c3.get_output();
            }
            if self.mixer2[3] {
                preamp2 += self.c4.get_output();
            }
            let val1 = preamp1 * (self.volume1 as f32 + 1.) / 8.;
            let val2 = preamp2 * (self.volume2 as f32 + 1.) / 8.;
            let out1 = self.alpha * (self.prev_out1 + val1 - self.prev1);
            let out2 = self.alpha * (self.prev_out2 + val2 - self.prev2);
            // push out1 and out2 into ring buffer
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0..=0xFF0F => panic!(),
            0xFF10 => {
                0x80 | self.c1.sweep_time << 4 & 0x70
                    | (self.c1.sweep_down as u8) << 3
                    | self.c1.sweep_shift & 0x07
            }
            0xFF11 => self.c1.duty << 6 & 0xC0 | 0x3F, // length is write-only
            0xFF12 => {
                self.c1.volume << 4 & 0xF0 | (self.c1.env_up as u8) << 3 | self.c1.env_slope & 0x07
            }
            0xFF13 => 0xFF, // frequncy is write only
            0xFF14 => (self.c1.use_length as u8) << 6 | 0xBF, // restart and frequency are read only
            0xFF15 => 0xFF, // unused
            0xFF16 => self.c2.duty << 6 & 0xC0 | 0x3F, // length is write-only
            0xFF17 => {
                self.c2.volume << 4 & 0xF0 | (self.c2.env_up as u8) << 3 | self.c2.env_slope & 0x07
            }
            0xFF18 => 0xFF, // frequncy is write only
            0xFF19 => (self.c2.use_length as u8) << 6 | 0xBF, // restart and frequency are read only
            0xFF1A => (self.c3.active as u8) << 7 | 0x7F,
            0xFF1B => 0xFF, // length is write-only
            0xFF1C => 0x9F | self.c3.level << 5 & 0x60,
            0xFF1D => 0xFF, // frequncy is write only
            0xFF1E => (self.c3.use_length as u8) << 6 | 0xBF, // restart and frequency are read only
            0xFF1F => 0xFF, // unused
            0xFF20 => 0xFF, // length is write-only
            0xFF21 => {
                self.c4.volume << 4 & 0xF0 | (self.c4.env_up as u8) << 3 | self.c4.env_slope & 0x07
            }
            0xFF22 => {
                self.c4.freq_shift << 4 & 0xF0
                    | (self.c4.lfsr_width as u8) << 3
                    | self.c4.freq_div & 0x07
            }
            0xFF23 => (self.c4.use_length as u8) << 6 | 0xBF, // restart is read only
            0xFF24 => 0x88 | self.volume2 << 4 & 0x70 | self.volume1 & 0x07,
            0xFF25 => {
                (self.mixer2[3] as u8) << 7
                    | (self.mixer2[2] as u8) << 6
                    | (self.mixer2[1] as u8) << 5
                    | (self.mixer2[0] as u8) << 4
                    | (self.mixer1[3] as u8) << 3
                    | (self.mixer1[2] as u8) << 2
                    | (self.mixer1[1] as u8) << 1
                    | (self.mixer1[0] as u8)
            }
            0xFF26 => {
                (self.master_enable as u8) << 7
                    | (self.c4.is_sound_on() as u8) << 3
                    | (self.c3.is_sound_on() as u8) << 2
                    | (self.c2.is_sound_on() as u8) << 1
                    | self.c1.is_sound_on() as u8
            }
            0xFF27..=0xFF2F => 0xFF, // unused
            0xFF30..=0xFF3F => self.c3.wave_pattern[(addr & 0x000F) as usize],
            0xFF40..=0xFFFF => panic!(),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0..=0xFF0F => panic!(),
            0xFF10 => {
                self.c1.sweep_time = val >> 4 & 0x07;
                self.c1.sweep_down = val & 0x08 != 0;
                self.c1.sweep_shift = val & 0x07;
            }
            0xFF11 => {
                self.c1.duty = val >> 6 & 0x03;
                self.c1.length = val & 0x3F;
                self.c1.length_changed();
            }
            0xFF12 => {
                self.c1.volume = val >> 4 & 0x0F;
                self.c1.env_up = val & 0x08 != 0;
                self.c1.env_slope = val & 0x07;
            }
            0xFF13 => {
                self.c1.frequency = self.c1.frequency & 0xFF00 | val as u16;
                self.c1.frequency_changed();
            }
            0xFF14 => {
                self.c1.use_length = val & 0x40 != 0;
                self.c1.frequency = self.c1.frequency & 0x00FF | ((val & 0x03) as u16) << 8;
                self.c1.frequency_changed();
                if val & 0x80 != 0 {
                    self.c1.restart();
                }
            }
            0xFF15 => (), // unused
            0xFF16 => {
                self.c2.duty = val >> 6 & 0x03;
                self.c2.length = val & 0x3F;
                self.c2.length_changed();
            }
            0xFF17 => {
                self.c2.volume = val >> 4 & 0x0F;
                self.c2.env_up = val & 0x08 != 0;
                self.c2.env_slope = val & 0x07;
            }
            0xFF18 => {
                self.c2.frequency = self.c2.frequency & 0xFF00 | val as u16;
                self.c2.frequency_changed();
            }
            0xFF19 => {
                self.c2.use_length = val & 0x40 != 0;
                self.c2.frequency = self.c2.frequency & 0x00FF | ((val & 0x03) as u16) << 8;
                self.c2.frequency_changed();
                if val & 0x80 != 0 {
                    self.c2.restart();
                }
            }
            0xFF1A => {
                self.c3.active = val & 0x80 != 0;
            }
            0xFF1B => {
                self.c3.length = val;
                self.c3.length_changed();
            }
            0xFF1C => {
                self.c3.level = val >> 5 & 0x03;
            }
            0xFF1D => {
                self.c3.frequency = self.c3.frequency & 0xFF00 | val as u16;
                self.c3.frequency_changed();
            }
            0xFF1E => {
                self.c3.use_length = val & 0x40 != 0;
                self.c3.frequency = self.c3.frequency & 0x00FF | ((val & 0x03) as u16) << 8;
                self.c3.frequency_changed();
                if val & 0x80 != 0 {
                    self.c3.restart();
                }
            }
            0xFF1F => (), // unused
            0xFF20 => {
                self.c4.length = val & 0x3F;
                self.c4.length_changed();
            }
            0xFF21 => {
                self.c4.volume = val >> 4 & 0x0F;
                self.c4.env_up = val & 0x08 != 0;
                self.c4.env_slope = val & 0x07;
            }
            0xFF22 => {
                self.c4.freq_shift = val >> 4 & 0x0F;
                self.c4.lfsr_width = val & 0x08 != 0;
                self.c4.freq_div = val & 0x07;
                self.c4.frequency_changed();
            }
            0xFF23 => {
                self.c4.use_length = val & 0x40 != 0;
                if val & 0x80 != 0 {
                    self.c4.restart();
                }
            }
            0xFF24 => {
                self.volume2 = val >> 4 & 0x07;
                self.volume1 = val & 0x07;
            }
            0xFF25 => {
                for c in 0..4 {
                    self.mixer2[c] = val & 1 << (c + 4) != 0;
                    self.mixer1[c] = val & 1 << c != 0;
                }
            }
            0xFF26 => {
                self.master_enable = val & 0x80 != 0;
            }
            0xFF27..=0xFF2F => (), // unused
            0xFF30..=0xFF3F => {
                self.c3.wave_pattern[(addr & 0x000F) as usize] = val;
            }
            0xFF40..=0xFFFF => panic!(),
        }
    }
}

struct Channel1 {
    sweep_time: u8,
    sweep_down: bool,
    sweep_shift: u8,
    duty: u8,
    length: u8,
    volume: u8,
    env_up: bool,
    env_slope: u8,
    frequency: u16,
    use_length: bool,
}

impl Channel1 {
    fn new() -> Self {
        Channel1 {
            sweep_time: 0,
            sweep_down: false,
            sweep_shift: 0,
            duty: 0,
            length: 0,
            volume: 0,
            env_up: false,
            env_slope: 0,
            frequency: 0,
            use_length: false,
        }
    }

    fn tick(&mut self) {}

    fn get_output(&self) -> f32 {
        0.
    }

    fn length_changed(&mut self) {}

    fn frequency_changed(&mut self) {}

    fn restart(&mut self) {}

    fn is_sound_on(&self) -> bool {
        false
    }
}

struct Channel2 {
    duty: u8,
    length: u8,
    volume: u8,
    env_up: bool,
    env_slope: u8,
    frequency: u16,
    use_length: bool,
}

impl Channel2 {
    fn new() -> Self {
        Channel2 {
            duty: 0,
            length: 0,
            volume: 0,
            env_up: false,
            env_slope: 0,
            frequency: 0,
            use_length: false,
        }
    }

    fn tick(&mut self) {}

    fn get_output(&self) -> f32 {
        0.
    }

    fn length_changed(&mut self) {}

    fn frequency_changed(&mut self) {}

    fn restart(&mut self) {}

    fn is_sound_on(&self) -> bool {
        false
    }
}

struct Channel3 {
    active: bool,
    length: u8,
    level: u8,
    frequency: u16,
    use_length: bool,
    wave_pattern: [u8; 16],
}

impl Channel3 {
    fn new() -> Self {
        Channel3 {
            active: false,
            length: 0,
            level: 0,
            frequency: 0,
            use_length: false,
            wave_pattern: [0; 16],
        }
    }

    fn tick(&mut self) {}

    fn get_output(&self) -> f32 {
        0.
    }

    fn length_changed(&mut self) {}

    fn frequency_changed(&mut self) {}

    fn restart(&mut self) {}

    fn is_sound_on(&self) -> bool {
        false
    }
}

struct Channel4 {
    length: u8,
    volume: u8,
    env_up: bool,
    env_slope: u8,
    freq_shift: u8,
    lfsr_width: bool,
    freq_div: u8,
    use_length: bool,
}

impl Channel4 {
    fn new() -> Self {
        Channel4 {
            length: 0,
            volume: 0,
            env_up: false,
            env_slope: 0,
            freq_shift: 0,
            lfsr_width: false,
            freq_div: 0,
            use_length: false,
        }
    }

    fn tick(&mut self) {}

    fn get_output(&self) -> f32 {
        0.
    }

    fn length_changed(&mut self) {}

    fn frequency_changed(&mut self) {}

    fn restart(&mut self) {}

    fn is_sound_on(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use cpal::{
        traits::{DeviceTrait, HostTrait, StreamTrait},
        BufferSize, OutputCallbackInfo, StreamConfig,
    };
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
        let mut stream_config: StreamConfig = config.clone().into();
        stream_config.buffer_size = BufferSize::Fixed(100);
        let stream = device
            .build_output_stream(&stream_config, output_fn, err_fn)
            .expect("failed to build output stream");
        stream.play().unwrap();
        std::thread::sleep(std::time::Duration::from_secs(1));
        *freq.lock().unwrap() = 1899;
        std::thread::sleep(std::time::Duration::from_secs(1));
        stream.pause().unwrap();
    }
}
