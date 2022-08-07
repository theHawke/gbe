use std::ops::{Add, Mul};
use ringbuf::{Consumer, Producer, RingBuffer};

const SAMPLING_SHIFT: i32 = 6;
const SAMPLING_MASK: u64 = (1 << SAMPLING_SHIFT) - 1;
pub const RINGBUF_RATE: usize = 1 << 22 - SAMPLING_SHIFT;
pub const RINGBUF_DT: f32 = 1. / RINGBUF_RATE as f32;
pub const RINGBUF_SIZE: usize = 4096;

pub trait AudioBackend {
    fn setup_stream(&mut self, queue: Consumer<Frame>);
}

#[derive(Clone, Copy, PartialEq)]
pub struct Frame(pub f32, pub f32);

impl Mul<f32> for Frame {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self::Output {
        Frame(self.0 * rhs, self.1 * rhs)
    }
}

impl Add for Frame {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Frame(self.0 + rhs.0, self.1 + rhs.1)
    }
}

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
    soundbuf: Producer<Frame>,
    _backend: Box<dyn AudioBackend>,
}

impl SoundController {

    pub fn new(mut backend: Box<dyn AudioBackend>) -> Self {
        let buf = RingBuffer::new(RINGBUF_SIZE);
        let (producer, consumer) = buf.split();
        backend.setup_stream(consumer);
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
            alpha: 0.98,
            soundbuf: producer,
            _backend: backend,
        }
    }

    pub fn tick(&mut self) {
        self.ticks += 1;
        if self.ticks & 0x01 == 0 {
            // 2 MHz
            self.c3.wave_channel.freq_clock();
        }
        if self.ticks & 0x03 == 0 {
            // 1 Mhz
            self.c1.square_wave.freq_clock();
            self.c2.square_wave.freq_clock();
        }
        if self.ticks & 0x07 == 0 {
            // 512 kHz
            self.c4.noise_lfsr.freq_clock();
        }
        if self.ticks & 0x3FFF == 0 {
            // 256 Hz
            self.c1.length_counter.length_clock();
            self.c2.length_counter.length_clock();
            self.c3.length_counter.length_clock();
        }
        if self.ticks & 0x7FFF == 0 {
            // 128 Hz
            self.c1.sweep_clock();
        }
        if self.ticks & 0xFFFF == 0 {
            // 64 Hz
            self.c1.volume_envelope.env_clock();
            self.c2.volume_envelope.env_clock();
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
            let val1 = preamp1 * (self.volume1 as f32 + 1.) / 16.;
            let val2 = preamp2 * (self.volume2 as f32 + 1.) / 16.;
            let out1 = self.alpha * (self.prev_out1 + val1 - self.prev1);
            let out2 = self.alpha * (self.prev_out2 + val2 - self.prev2);
            while let Err(_) = self.soundbuf.push(Frame(out1, out2)) {
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            self.prev1 = val1;
            self.prev2 = val2;
            self.prev_out1 = out1;
            self.prev_out2 = out2;
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0..=0xFF0F => panic!(),
            0xFF10 => {
                0x80 | self.c1.frequency_sweep.sweep << 4 & 0x70
                    | (self.c1.frequency_sweep.sweep_down as u8) << 3
                    | self.c1.frequency_sweep.sweep_shift & 0x07
            }
            0xFF11 => self.c1.square_wave.duty << 6 & 0xC0 | 0x3F, // length is write-only
            0xFF12 => {
                self.c1.volume_envelope.volume << 4 & 0xF0
                    | (self.c1.volume_envelope.env_up as u8) << 3
                    | self.c1.volume_envelope.envelope & 0x07
            }
            0xFF13 => 0xFF, // frequncy is write only
            0xFF14 => (self.c1.length_counter.use_length as u8) << 6 | 0xBF, // restart and frequency are read only
            0xFF15 => 0xFF,                                                  // unused
            0xFF16 => self.c2.square_wave.duty << 6 & 0xC0 | 0x3F,           // length is write-only
            0xFF17 => {
                self.c2.volume_envelope.volume << 4 & 0xF0
                    | (self.c2.volume_envelope.env_up as u8) << 3
                    | self.c2.volume_envelope.envelope & 0x07
            }
            0xFF18 => 0xFF, // frequncy is write only
            0xFF19 => (self.c2.length_counter.use_length as u8) << 6 | 0xBF, // restart and frequency are read only
            0xFF1A => (self.c3.active as u8) << 7 | 0x7F,
            0xFF1B => 0xFF, // length is write-only
            0xFF1C => 0x9F | self.c3.wave_channel.level << 5 & 0x60,
            0xFF1D => 0xFF, // frequncy is write only
            0xFF1E => (self.c3.length_counter.use_length as u8) << 6 | 0xBF, // restart and frequency are read only
            0xFF1F => 0xFF,                                                  // unused
            0xFF20 => 0xFF,                                                  // length is write-only
            0xFF21 => {
                self.c4.volume_envelope.volume << 4 & 0xF0 | (self.c4.volume_envelope.env_up as u8) << 3 | self.c4.volume_envelope.envelope & 0x07
            }
            0xFF22 => {
                self.c4.noise_lfsr.freq_shift << 4 & 0xF0
                    | (self.c4.noise_lfsr.narrow_lfsr as u8) << 3
                    | self.c4.noise_lfsr.freq_div & 0x07
            }
            0xFF23 => (self.c4.length_counter.use_length as u8) << 6 | 0xBF, // restart is read only
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
            0xFF30..=0xFF3F => self.c3.wave_channel.wave_pattern[(addr & 0x000F) as usize],
            0xFF40..=0xFFFF => panic!(),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0..=0xFF0F => panic!(),
            0xFF10 => {
                self.c1.frequency_sweep.sweep = val >> 4 & 0x07;
                self.c1.frequency_sweep.sweep_down = val & 0x08 != 0;
                self.c1.frequency_sweep.sweep_shift = val & 0x07;
            }
            0xFF11 => {
                self.c1.square_wave.duty = val >> 6 & 0x03;
                self.c1.length_counter.write_length(val & 0x3F);
            }
            0xFF12 => {
                self.c1.volume_envelope.volume = val >> 4 & 0x0F;
                self.c1.volume_envelope.env_up = val & 0x08 != 0;
                self.c1.volume_envelope.envelope = val & 0x07;
            }
            0xFF13 => {
                self.c1.square_wave.frequency = self.c1.square_wave.frequency & 0xFF00 | val as u16;
            }
            0xFF14 => {
                self.c1.length_counter.use_length = val & 0x40 != 0;
                self.c1.square_wave.frequency =
                    self.c1.square_wave.frequency & 0x00FF | ((val & 0x07) as u16) << 8;
                if val & 0x80 != 0 {
                    self.c1.restart();
                }
            }
            0xFF15 => (), // unused
            0xFF16 => {
                self.c2.square_wave.duty = val >> 6 & 0x03;
                self.c2.length_counter.write_length(val & 0x3F);
            }
            0xFF17 => {
                self.c2.volume_envelope.volume = val >> 4 & 0x0F;
                self.c2.volume_envelope.env_up = val & 0x08 != 0;
                self.c2.volume_envelope.envelope = val & 0x07;
            }
            0xFF18 => {
                self.c2.square_wave.frequency = self.c2.square_wave.frequency & 0xFF00 | val as u16;
            }
            0xFF19 => {
                self.c2.length_counter.use_length = val & 0x40 != 0;
                self.c2.square_wave.frequency =
                    self.c2.square_wave.frequency & 0x00FF | ((val & 0x07) as u16) << 8;
                if val & 0x80 != 0 {
                    self.c2.restart();
                }
            }
            0xFF1A => {
                self.c3.active = val & 0x80 != 0;
            }
            0xFF1B => {
                self.c3.length_counter.write_length(val);
            }
            0xFF1C => {
                self.c3.wave_channel.level = val >> 5 & 0x03;
            }
            0xFF1D => {
                self.c3.wave_channel.frequency = self.c3.wave_channel.frequency & 0xFF00 | val as u16;
            }
            0xFF1E => {
                self.c3.length_counter.use_length = val & 0x40 != 0;
                self.c3.wave_channel.frequency = self.c3.wave_channel.frequency & 0x00FF | ((val & 0x07) as u16) << 8;
                if val & 0x80 != 0 {
                    self.c3.restart();
                }
            }
            0xFF1F => (), // unused
            0xFF20 => {
                self.c4.length_counter.write_length(val & 0x3F);
            }
            0xFF21 => {
                self.c4.volume_envelope.volume = val >> 4 & 0x0F;
                self.c4.volume_envelope.env_up = val & 0x08 != 0;
                self.c4.volume_envelope.envelope = val & 0x07;
            }
            0xFF22 => {
                self.c4.noise_lfsr.freq_shift = val >> 4 & 0x0F;
                self.c4.noise_lfsr.narrow_lfsr = val & 0x08 != 0;
                self.c4.noise_lfsr.freq_div = val & 0x07;
            }
            0xFF23 => {
                self.c4.length_counter.use_length = val & 0x40 != 0;
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
                self.c3.wave_channel.wave_pattern[(addr & 0x000F) as usize] = val;
            }
            0xFF40..=0xFFFF => panic!(),
        }
    }
}

struct LengthCounter<const S: u16> {
    use_length: bool,
    length_timer: u16,
}

impl<const S: u16> LengthCounter<S> {
    fn new() -> Self {
        LengthCounter {
            use_length: false,
            length_timer: S,
        }
    }

    fn length_clock(&mut self) {
        if self.use_length && self.length_timer > 0 {
            self.length_timer -= 1;
        }
    }

    fn write_length(&mut self, length: u8) {
        self.length_timer = S - length as u16;
    }

    fn reset(&mut self) {
        if self.length_timer == 0 {
            self.length_timer = S;
        }
    }

    fn length_on(&self) -> bool {
        self.length_timer != 0
    }
}

struct SquareWave {
    duty: u8,
    frequency: u16,
    freq_timer: u16,
    duty_step: u8,
}

impl SquareWave {
    fn new() -> Self {
        SquareWave {
            duty: 0,
            frequency: 0,
            freq_timer: 2048,
            duty_step: 0,
        }
    }

    fn freq_clock(&mut self) {
        self.freq_timer -= 1;
        if self.freq_timer == 0 {
            self.freq_timer = 2048 - self.frequency;
            self.duty_step += 1;
            self.duty_step &= 0x07;
        }
    }

    fn reset(&mut self) {
        self.freq_timer = 2048 - self.frequency;
    }

    const DUTY_PATTERNS: [[bool; 8]; 4] = [
        [false, false, false, false, false, false, false, true],
        [true, false, false, false, false, false, false, true],
        [true, false, false, false, false, true, true, true],
        [false, true, true, true, true, true, true, false],
    ];

    fn wave_output(&self) -> bool {
        Self::DUTY_PATTERNS[self.duty as usize][self.duty_step as usize]
    }
}

struct VolumeEnvelope {
    volume: u8,
    volume_out: u8,
    env_up: bool,
    envelope: u8,
    env_timer: u8,
    env_done: bool,
}

impl VolumeEnvelope {
    fn new() -> Self {
        VolumeEnvelope {
            volume: 0,
            volume_out: 0,
            env_up: false,
            envelope: 0,
            env_timer: 0,
            env_done: true,
        }
    }

    fn env_clock(&mut self) {
        if self.envelope != 0 && !self.env_done {
            self.env_timer -= 1;
            if self.env_timer == 0 {
                self.env_timer = self.envelope;
                if self.env_up && self.volume_out < 15 {
                    self.volume_out += 1;
                } else if !self.env_up && self.volume_out > 0 {
                    self.volume_out -= 1;
                } else {
                    self.env_done = true;
                }
            }
        }
    }

    fn output(&self, wave: bool) -> u8 {
        if wave {
            self.volume_out
        } else {
            0
        }
    }

    fn dac_on(&self) -> bool {
        self.volume != 0 || self.env_up
    }

    fn reset(&mut self) {
        self.volume_out = self.volume;
        self.env_timer = self.envelope;
        self.env_done = false;
    }
}

struct FrequencySweep {
    sweep: u8,
    sweep_down: bool,
    sweep_shift: u8,
    enable: bool,
    sweep_timer: u8,
    shadow_freq: u16,
    channel_disable: bool,
}

impl FrequencySweep {
    fn new() -> Self {
        FrequencySweep {
            sweep: 0,
            sweep_down: false,
            sweep_shift: 0,
            enable: false,
            sweep_timer: 0,
            shadow_freq: 0,
            channel_disable: false,
        }
    }

    fn sweep_clock(&mut self, frequency_out: &mut u16) {
        if self.enable && self.sweep_timer > 0 {
            self.sweep_timer -= 1;
            if self.sweep_timer == 0 && self.sweep != 0 {
                self.sweep_timer = self.sweep;
                let new = self.calculate_new();
                if self.overflow_check(new) {
                    self.shadow_freq = new;
                    *frequency_out = new;
                    self.overflow_check(self.calculate_new());
                }
            }
        }
    }

    fn calculate_new(&self) -> u16 {
        if self.sweep_shift != 0 {
            if self.sweep_down {
                self.shadow_freq
                    .wrapping_sub(self.shadow_freq >> self.sweep_shift)
            } else {
                self.shadow_freq + (self.shadow_freq >> self.sweep_shift)
            }
        } else {
            self.shadow_freq
        }
    }

    fn overflow_check(&mut self, new: u16) -> bool {
        if new > 2047 {
            self.channel_disable = true;
            false
        } else {
            true
        }
    }

    fn reset(&mut self, frequency: &u16) {
        self.shadow_freq = *frequency;
        self.sweep_timer = self.sweep;
        self.enable = self.sweep != 0 || self.sweep_shift != 0;
        if self.sweep_shift != 0 {
            self.overflow_check(self.calculate_new());
        }
        self.channel_disable = false;
    }

    fn channel_on(&self) -> bool {
        !self.channel_disable
    }
}

fn dac(on: bool, val: u8) -> f32 {
    if on {
        2. * val as f32 / 15. - 1.
    } else {
        0.
    }
}

struct Channel1 {
    frequency_sweep: FrequencySweep,
    square_wave: SquareWave,
    volume_envelope: VolumeEnvelope,
    length_counter: LengthCounter<64>,
}

impl Channel1 {
    fn new() -> Self {
        Channel1 {
            frequency_sweep: FrequencySweep::new(),
            square_wave: SquareWave::new(),
            volume_envelope: VolumeEnvelope::new(),
            length_counter: LengthCounter::new(),
        }
    }

    fn sweep_clock(&mut self) {
        self.frequency_sweep
            .sweep_clock(&mut self.square_wave.frequency);
    }

    fn get_output(&self) -> f32 {
        let val = if self.is_sound_on() {
            self.volume_envelope.output(self.square_wave.wave_output())
        } else {
            0
        };
        dac(self.volume_envelope.dac_on(), val)
    }

    fn restart(&mut self) {
        self.length_counter.reset();
        self.square_wave.reset();
        self.volume_envelope.reset();
        self.frequency_sweep.reset(&self.square_wave.frequency)
    }

    fn is_sound_on(&self) -> bool {
        self.length_counter.length_on()
            && self.frequency_sweep.channel_on()
            && self.volume_envelope.dac_on()
    }
}

struct Channel2 {
    square_wave: SquareWave,
    volume_envelope: VolumeEnvelope,
    length_counter: LengthCounter<64>,
}

impl Channel2 {
    fn new() -> Self {
        Channel2 {
            square_wave: SquareWave::new(),
            volume_envelope: VolumeEnvelope::new(),
            length_counter: LengthCounter::new(),
        }
    }

    fn get_output(&self) -> f32 {
        let val = if self.is_sound_on() {
            self.volume_envelope.output(self.square_wave.wave_output())
        } else {
            0
        };
        dac(self.volume_envelope.dac_on(), val)
    }

    fn restart(&mut self) {
        self.length_counter.reset();
        self.square_wave.reset();
        self.volume_envelope.reset();
    }

    fn is_sound_on(&self) -> bool {
        self.length_counter.length_on() && self.volume_envelope.dac_on()
    }
}

struct WaveChannel {
    level: u8,
    frequency: u16,
    wave_pattern: [u8; 16],
    freq_timer: u16,
    wave_pos: u8,
}

impl WaveChannel {
    fn new() -> Self {
        WaveChannel {
            level: 0,
            frequency: 0,
            wave_pattern: [0; 16],
            freq_timer: 2048,
            wave_pos: 0,
        }
    }

    fn freq_clock(&mut self) {
        self.freq_timer -= 1;
        if self.freq_timer == 0 {
            self.freq_timer = 2048 - self.frequency;
            self.wave_pos += 1;
            self.wave_pos &= 0x1F;
        }
    }

    fn get_sample(&self) -> u8 {
        let b = self.wave_pattern[(self.wave_pos >> 1) as usize];
        // two samples per byte, first sample in high nybble
        if self.wave_pos & 1 == 0 {
            b >> 4
        } else {
            b & 0x0F
        }
    }

    fn output(&self) -> u8 {
        if self.level != 0 {
            self.get_sample() >> (self.level - 1)
        } else {
            0
        }
    }

    fn reset(&mut self) {
        self.freq_timer = 2048 - self.frequency;
        self.wave_pos = 0;
    }

    fn sound_on(&self) -> bool {
        self.level != 0
    }
}

struct Channel3 {
    active: bool,
    length_counter: LengthCounter<256>,
    wave_channel: WaveChannel,
}

impl Channel3 {
    fn new() -> Self {
        Channel3 {
            active: false,
            length_counter: LengthCounter::new(),
            wave_channel: WaveChannel::new(),
        }
    }

    fn get_output(&self) -> f32 {
        dac(self.active, self.wave_channel.output())
    }

    fn restart(&mut self) {
        self.length_counter.reset();
        self.wave_channel.reset();
    }

    fn is_sound_on(&self) -> bool {
        self.active && self.length_counter.length_on() && self.wave_channel.sound_on()
    }
}

struct NoiseLfsr {
    freq_shift: u8,
    narrow_lfsr: bool,
    freq_div: u8,
    lfsr: u16,
    freq_timer: u16,
}

impl NoiseLfsr {
    fn new() -> Self {
        NoiseLfsr {
            freq_shift: 0,
            narrow_lfsr: false,
            freq_div: 0,
            lfsr: 0xFFFF,
            freq_timer: 1,
        }
    }

    fn period(&self) -> u16 {
        if self.freq_div == 0 {
            1 << self.freq_shift
        } else {
            (self.freq_div as u16) << (self.freq_shift + 1)
        }
    }

    fn freq_clock(&mut self) {
        self.freq_timer -= 1;
        if self.freq_timer == 0 {
            self.freq_timer = self.period();
            let new = self.lfsr ^ (self.lfsr >> 1);
            self.lfsr >>= 1;
            self.lfsr |= new << 14;
            if self.narrow_lfsr {
                self.lfsr |= new << 6;
            }
        }
    }

    fn noise_output(&self) -> bool {
        self.lfsr & 0x01 != 0
    }

    fn reset(&mut self) {
        self.freq_timer = self.period();
        self.lfsr = 0xFFFF;
    }
}

struct Channel4 {
    length_counter: LengthCounter<64>,
    volume_envelope: VolumeEnvelope,
    noise_lfsr: NoiseLfsr,
}

impl Channel4 {
    fn new() -> Self {
        Channel4 {
            length_counter: LengthCounter::new(),
            volume_envelope: VolumeEnvelope::new(),
            noise_lfsr: NoiseLfsr::new(),
        }
    }

    fn get_output(&self) -> f32 {
        let val = if self.is_sound_on() {
            self.volume_envelope.output(self.noise_lfsr.noise_output())
        } else {
            0
        };
        dac(self.volume_envelope.dac_on(), val)
    }

    fn restart(&mut self) {
        self.length_counter.reset();
        self.volume_envelope.reset();
        self.noise_lfsr.reset();
    }

    fn is_sound_on(&self) -> bool {
        self.length_counter.length_on() && self.volume_envelope.dac_on()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app::audio_backend::CpalBackend;

    #[test]
    fn test_startup_sound() {
        let backend = Box::new(CpalBackend::new());
        let mut sc = SoundController::new(backend);
        sc.write(0xFF26, 0x80); // enable
        sc.write(0xFF24, 0x77); // L/R volume
        sc.write(0xFF25, 0x11); // L/R mix
        sc.write(0xFF11, 0x80); // C1 duty / length
        sc.write(0xFF12, 0xF3); // C1 volume / envelope
        sc.write(0xFF13, 0x83); // C1 freq low
        sc.write(0xFF14, 0x87); // C1 restart / freq high

        for _ in 0..1 << 19 {
            sc.tick();
        }

        sc.write(0xFF13, 0xC1);
        sc.write(0xFF14, 0x87);

        for _ in 0..1 << 22 {
            sc.tick();
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    #[test]
    fn test_wave_channel() {
        let backend = Box::new(CpalBackend::new());
        let mut sc = SoundController::new(backend);
        let f = 1899;
        sc.write(0xFF26, 0x80); // enable
        sc.write(0xFF24, 0x22); // L/R volume
        sc.write(0xFF25, 0x44); // L/R mix
        sc.write(0xFF1A, 0x80); // C3 enable
        sc.write(0xFF1C, 0x20); // C3 level
        for i in 0..15 {
            sc.write(0xFF30 + i, (i | i << 4) as u8); // C3 wave pattern (sawtooth wave)
        }
        sc.write(0xFF1D, f as u8); // C3 freq low
        sc.write(0xFF1E, 0x80 | (f >> 8) as u8); // C3 restart / freq high

        for _ in 0..1 << 22 {
            sc.tick();
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    #[test]
    fn test_noise_channel() {
        let backend = Box::new(CpalBackend::new());
        let mut sc = SoundController::new(backend);
        sc.write(0xFF26, 0x80); // enable
        sc.write(0xFF24, 0x22); // L/R volume
        sc.write(0xFF25, 0x88); // L/R mix
        sc.write(0xFF21, 0xF0); // C4 volume / envelope
        sc.write(0xFF22, 0x47); // C4 lfsr
        sc.write(0xFF23, 0x80); // C4 restart

        for _ in 0..1 << 22 {
            sc.tick();
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
}
