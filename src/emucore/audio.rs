use std::sync::{Arc, Mutex};

struct SoundController {}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
struct AudioCR {
    c1_sweep_time: u8,
    c1_sweep_down: bool,
    c1_sweep_shift: u8,
    c1_duty: u8,
    c1_length: u8,
    c1_volume: u8,
    c1_env_up: bool,
    c1_env_slope: u8,
    c1_frequency: u16,
    c1_use_length: bool,
    c1_restart: bool,
    c2_duty: u8,
    c2_length: u8,
    c2_volume: u8,
    c2_env_up: bool,
    c2_env_slope: u8,
    c2_frequency: u16,
    c2_use_length: bool,
    c2_restart: bool,
    c3_active: bool,
    c3_length: u8,
    c3_level: u8,
    c3_frequency: u16,
    c3_use_length: bool,
    c3_restart: bool,
    c3_wave_pattern: [u8; 16],
    c4_length: u8,
    c4_volume: u8,
    c4_env_up: bool,
    c4_env_slope: u8,
    c4_freq_shift: u8,
    c4_lfsr_width: bool,
    c4_freq_div: u8,
    c4_use_length: bool,
    c4_restart: bool,
    vin_to_out2: bool,
    out2_volume: u8,
    vin_to_out1: bool,
    out1_volume: u8,
    c4_to_out2: bool,
    c3_to_out2: bool,
    c2_to_out2: bool,
    c1_to_out2: bool,
    c4_to_out1: bool,
    c3_to_out1: bool,
    c2_to_out1: bool,
    c1_to_out1: bool,
    all_on_off: bool,
    c4_on_flag: bool,
    c3_on_flag: bool,
    c2_on_flag: bool,
    c1_on_flag: bool,
}
struct SoundGenerator {
    cr: Arc<Mutex<AudioCR>>,
    sample_num: u32,
    sample_rate: u32,
    previous_output: f32,
    previous_sample: f32,
    alpha: f32,
}

impl SoundGenerator {
    fn next_sample(&mut self) -> f32 {
        let t = self.sample_num as f32 / self.sample_rate as f32;
        let period = (2048 - self.cr.lock().unwrap().c1_frequency) as f32 / 131072.;
        let v = 0.2 * (((t % period < period / 2.) as i32) * 2 - 1) as f32;
        self.sample_num += 1;
        let out = self.alpha * (self.previous_sample + v - self.previous_output);
        self.previous_output = v;
        self.previous_sample = out;
        out
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use cpal::{
        traits::{DeviceTrait, HostTrait, StreamTrait},
        BufferSize, OutputCallbackInfo, StreamConfig, SupportedBufferSize,
    };

    #[test]
    fn test_cpal() {
        let host = cpal::default_host();
        let device = host.default_output_device().unwrap();
        let config = device.default_output_config().unwrap();
        let err_fn = |err| eprintln!("an error occurred on the output audio stream: {}", err);
        let cr = Arc::new(Mutex::new(AudioCR::default()));
        let mut sound_generator = SoundGenerator {
            sample_num: 0,
            sample_rate: config.sample_rate().0,
            cr: cr.clone(),
            previous_output: 0.,
            previous_sample: 0.,
            alpha: 0.95,
        };
        cr.lock().unwrap().c1_frequency = 1750;
        let mut num_calls = 0;
        let output_fn = move |data: &mut [f32], info: &OutputCallbackInfo| {
            println!(
                "call {}, {} samples at {:?}",
                num_calls,
                data.len(),
                info.timestamp().playback
            );
            for i in 0..data.len() / 2 {
                let s = sound_generator.next_sample();
                data[2 * i] = s;
                data[2 * i + 1] = s;
            }
            num_calls += 1;
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
        cr.lock().unwrap().c1_frequency = 1899;
        std::thread::sleep(std::time::Duration::from_secs(1));
        stream.pause().unwrap();
    }
}
