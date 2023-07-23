

/*
 * Apu will be ticked 4 times every cpu cycle just like the Ppu
 *
 * The algorithm for ppu is to tick all 4 channels, 2 pulse, 1 custom wave, 1 noise
 * this will produce an output, which will then be "mixed" and outputed by the apu
 *
 * This output will be buffered up and output once per "frame" which is 60 hz
 */
pub struct Apu {

    pub on: u8,             // 0xFF26       NR52
                                 //  Bit 7      All On/Off
                                 //  Bit 6-4    Unused 
                                 //  Bit 3     Channel 4 On (Read only)
                                 //  Bit 2     Channel 3 On (Read only)
                                 //  Bit 1     Channel 2 On (Read only)
                                 //  Bit 0     Channel 1 On (Read only)

    pub panning: u8,        // 0xFF25      NR51
                                 // Bit 7       Mix Ch 4 left
                                 // Bit 6       Mix Ch 3 left
                                 // Bit 5       Mix Ch 2 left
                                 // Bit 4       Mix Ch 1 left
                                 // Bit 3       Mix Ch 4 right
                                 // Bit 2       Mix Ch 3 right
                                 // Bit 1       Mix Ch 2 right
                                 // Bit 0       Mix Ch 1 right

    pub volume_vin: u8,     // 0xFF24      NR50
                                 // Bit 7       Mix VIN left
                                 // Bit 6-4     Left volume 0-7
                                 // Bit 3       Mix VIN right
                                 // Bit 2-0     Right volume 0-7

    pub wave: WaveChannel,

    pub div_apu_countdown: u32,       //  The Apu ticks at the same rate as Ppu. So it should tick a rate of
                                      //  4 ticks per bus tick (bus is 4.2 Mhz. The DIV-APU will tick at a rate of 8192 bus
                                      //    ticks. This gives it a rate of 512 Hz.
                                      //
                                      //  This register should be initialize to 8192
                                      //    From Pandocs the following sound register tick:
                                      //          Envelope Sweep          once every 8 DIV-APU ticks (64 Hz)
                                      //          Sound Length            once every 2 DIV-APU ticks (256 Hz)
                                      //          CH1 freq sweep          once every 4 DIV-APU ticks (128 Hz)

    pub div_apu: u32            // This ticks at the Bus rate.
}

impl Apu {
    const DIV_APU_BUS_TICKS: u32 = 8192;

    pub fn tick(&mut self)  {
        self.div_apu_countdown = self.div_apu_countdown.saturating_sub(1);
        if self.div_apu_countdown == 0 { 
            self.div_apu_countdown = Apu::DIV_APU_BUS_TICKS; 
            self.div_apu = self.div_apu.wrapping_add(1);
        }

        let length_tick = self.div_apu % 2 == 0;
        self.wave.tick(length_tick);
    }

    pub fn read(&self, addr: u16) -> u8 {
        0
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0xFF1D | 0xFF1E | 0xFF30..=0xFF3F => {
                self.wave.write(addr, val);
            },
            _ => {
                panic!("Apu write to unknown address: {:04X}", addr);
            }
        }
    }
}

/* Wave Channel
 * This channel lets the user specify 32 samples 4-bits each.
 * The samples can be read at different rates, via the wavelength register
 */

#[derive(Debug, Default)]
pub struct WaveChannel {
    pub dac_on: bool,                  // 0xFF1A NR30 Channel 3 Enabled
    pub length: u8,                  // 0xFF1B NR31 Length (Write-only)                                  
                                     //     This is ticked at 256 Hz
                                     //     When the length reaches 64
                                     //     the channel is turned off. 
                                     //
                                     //     The value written is 256 - v.
                                     //     its decremented. when it reaches 0 it turns off
                                     
    pub volume: u8,                  // 0xFF1C NR32 Volume 
                                     //     0 - Mute            Shift 4 bits right
                                     //     1 - 100% Volume     Shift 0 bits right
                                     //     2 - 50% Volume      Shift 1 bits right
                                     //     3 - 25% Volume      Shift 2 bits right

    pub wavelength: u16,             // 0xFF1D NR33 
                                     //      Wavelength is 11 bit value, NR33 holds upper 8 bits
                                     //      The frequency of the 32 sample waveform is
                                     //      65536/(2048-wavelength). Which also means
                                     //      that each sample outputs   
                                     //      2097152/(2048-wavelength) frequency 
                                     //      Can also be written as 2/(2048-wavelength) Mhz 
                                     //      or every (2048-wavelength)/2 ticks. 
                                     //
                                     //      Frequency it 1/4 hz  means that we output ticks 4/1
                                     //      ticks
                                     
                                     //      Bits 3-8 of wavelength
                                     //      The lower 3 bits are in 0xFF1E NR34 
                                     //      Bits 2-0  of wavelength

    pub wavelength_ctl: u8,          // 0xFF1E NR34 
                                     //      Bit 7  Trigger (Write-only)
                                     //      Bit 6  Sound  Length enabled 
                                     //       (1 = Stop when NR31 done)

   pub pattern: [u8; 16],            // 0xFF30-0xFF3F  
                                     //       Pattern goes from lower address to higher address
                                     //       4 bits for each sample
                                     //       When CH3 is started, the first sample is lower
                                     //       nibble of 0xFF30 not upper nibble.

   // Implementation details
   pub current_length: u8,          // How long before the Wave Channel turns off
   pub current_sample_idx: u8,      // Goes from 0 to 31. We will output
                                    // pattern[current_sample_idx/2] every time
                                    // wave_length_coutdown reaches 0. We will also 
                                    // extract the current bit pattern from pattern by testing
                                    // if current_sample_idx % 2 == 0, outputting upper bit,
                                    // otherwise putting lower bit 
                                    
   pub wavelength_countdown: u16,   // This is set to (2048-wavelength)/2. 
                                    // It is then decremented every tick. 
                                    // When it goes down to 0 then we output a WAVE RAM pattern

   pub cur_output: u8,              // Output of the wave
}

impl WaveChannel {
    pub fn tick(&mut self, is_length_ticked: bool) {
        self.wavelength_countdown = self.wavelength_countdown.saturating_sub(1); 
        if self.wavelength_countdown == 0 {
            // reset wavelength counter
            self.wavelength_countdown = (2048 - self.wavelength)/2;
            self.cur_output = self.tick_pos();
        }
       
        if self.is_length_enabled() && is_length_ticked {
            self.current_length = self.current_length.saturating_sub(1);
            if self.current_length == 0 {
                self.dac_on = false;
            }
        }
    }

    pub fn is_length_enabled(&self) -> bool {
        (self.wavelength_ctl >> 6)  & 0x1 == 1
    }

    pub fn tick_pos(&mut self) -> u8 { 
        // we should never output position 0 when it initially turns on.
        self.current_sample_idx = (self.current_sample_idx + 1) % 32;

        let val = self.pattern[self.current_sample_idx as usize / 2];
        if self.current_sample_idx % 2 == 0 {
            // upper 4 bit 
            val >> 4 
        } else {
            // lower 4 bits
            val & 0xF
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0xFF1D => {
                self.wavelength = (self.wavelength & 0xFF00) | val as u16;
            },

            0xFF1E => {
                self.wavelength = (self.wavelength & 0x00FF) | ((val as u16 & 0x3) << 8);
                // We only care about bits 7 and 6
                self.wavelength_ctl = val & 0xC0;
            },
            0xFF30..=0xFF3F => {
                self.pattern[(addr - 0xFF30) as usize] = val;
            },
            _ => {
                panic!("WaveChannel write to unknown address: {:04X}", addr);
            },
        }
    }
}
