use crate::util::Shared;
use crate::ppu::Ppu;
use crate::mapper::{Mbc, Mapper};
pub struct Bus {
    pub mem: Shared<Vec<u8>>,
    pub timer: Timer,
    pub interrupts: Shared<Interrupts>,
    pub ppu: Shared<Ppu>,
    pub mbc: Shared<Mbc>,
    pub input: InputButtons, 
    pub prev_input: InputButtons,
    pub boot_rom: [u8; 256],
    boot_rom_disabled: bool,
}


impl Bus {
    pub fn new(gb_rom: Shared<Vec<u8>>, boot_rom: &[u8], mem: Shared<Vec<u8>>, interrupts: Shared<Interrupts>, ppu: Shared<Ppu>)  -> Self {
        let mbc = Shared::new(Mbc::new(gb_rom.clone(), boot_rom));
        let mut bus =     Bus {
            mem: mem.clone(),
            timer: Timer::new(interrupts.clone()),
            interrupts: interrupts.clone(),
            ppu: ppu.clone(),
            mbc: mbc.clone(),
            input: Default::default(),
            prev_input: Default::default(),
            boot_rom: [0; 256],
            boot_rom_disabled: false,
        };
        bus.mem[0..gb_rom.len()].copy_from_slice(&gb_rom);
        bus.boot_rom[0..256].copy_from_slice(boot_rom);
        bus
    }

    pub fn new_without_bootrom(gb_rom: Shared<Vec<u8>>,  mem: Shared<Vec<u8>>, interrupts: Shared<Interrupts>, ppu: Shared<Ppu>)  -> Self {
        let mbc = Shared::new(Mbc::new_without_bootrom(gb_rom.clone()));
        let mut bus =     Bus {
            mem: mem.clone(),
            timer: Timer::new(interrupts.clone()),
            interrupts: interrupts.clone(),
            ppu: ppu.clone(),
            mbc: mbc.clone(),
            input: Default::default(),
            prev_input: Default::default(),
            boot_rom: [0;256],
            boot_rom_disabled: true,
        };
        bus.mem[0..gb_rom.len()].copy_from_slice(&gb_rom);
        bus
    }

    pub fn read8(&self, addr: u16) -> u8 {
        let Some(val) = self.mbc.read8(addr, self.boot_rom_disabled)  else {
            return match addr {
                // Ppu:  tile_data, lcd regs, wy and wx 
                0x8000..=0x9FFF | 0xFE00..=0xFE9F | 0xFF40..=0xFF45 | 0xFF47..=0xFF49 | 0xFF4A..=0xFF4B  => {
                    self.ppu.read8(addr)
                },

                // Buttons
                0xFF00 => {
                    if !self.input.not_select_action {
                        let button = !self.input.key_a as u8 | 
                                     (!self.input.key_b as u8) << 1 | 
                                     (!self.input.key_select as u8) << 2 |
                                     (!self.input.key_start as u8) << 3 | 
                                     (!self.input.not_select_direction as u8) << 4 | 
                                     (!self.input.not_select_action as u8) << 5;
                        button
                    } else {
                        let button = !self.input.key_right as u8 | 
                                     (!self.input.key_left as u8) << 1 | 
                                     (!self.input.key_up as u8) << 2 |
                                     (!self.input.key_down as u8) << 3 | 
                                     (!self.input.not_select_direction as u8) << 4 | 
                                     (!self.input.not_select_action as u8) << 5;
                        button
                    }
                },
                // Serial Data Transfer
                0xFF01..=0xFF02 => {
                    //println!("serial addr: {:04X}", addr);
                    0
                        //self.serial.read8(addr)
                },

                // Timer
                0xFF04..=0xFF07 => {
                    self.timer.read8(addr)
                },

                // Interrupt
                0xFFFF | 0xFF0F => {
                    self.interrupts.read8(addr)
                },

                0x00 ..= 0xFF => {
                    if !self.boot_rom_disabled {
                        //println!("boot rom read from mem: {:04X} val: {:02X}", addr, self.boot_rom[addr as usize]);
                        self.boot_rom[addr as usize]
                    } else {
                        //println!("boot rom skipped from mem: {:04X} val: {:02X}", addr, self.mem[addr as usize]);
                        self.mem[addr as usize]
                    }
                },
                _ => {
                    self.mem[addr as usize]
                }
            }
        };
        val
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        let Ok(_) =  self.mbc.write8(addr, val) else {
            match addr {
                0x8000..=0x9FFF | 0xFE00..=0xFE9F | 0xFF40..=0xFF43 | 0xFF45 | 0xFF47..=0xFF49 | 0xFF4A..=0xFF4B => {
                    self.ppu.write8(addr, val);
                },

                0xFF00 => {
                    let input = self.read8(0xFF00);
                    self.input.not_select_action = (val >> 5) & 1 == 1;
                    self.input.not_select_direction = (val >> 4) & 1 == 1;
                }

                0xFF01 => {
                    //println!("serial addr: {:04X}, val: {:02X}", addr, val);
                    let c = val as char;
                    match c {
                        'a'..='z' | 'A'..='Z' | ' ' | 
                        '\n' |'0'..='9' | ':' | '_' | 
                        '-' | ',' | '(' | ')' => {
                            print!("{}",c);
                        },
                        _ => {
                            println!("{}", c);
                            //println!("weird char: {}", c);
                        },
                    }
                    //    self.serial.write8(addr, val);
                },

                0xFF04..=0xFF07 => {
                    self.timer.write8(addr, val);
                },

                0xFF46 => {
                    // Get copy of data from $XX00 to $XX9F
                    let mut source = vec![];
                    let start_addr = (val as u16) << 8;

                    // TODO: can we pass in a slice directly instead of copying 
                    // the data to a vec?
                    for i in 0u16..=0x9F {
                        source.push(self.read8(start_addr + i))
                    }
                    self.ppu.write_oam_dma(val, &source[..]);
                },

                0xFF50 => {
                    println!("boot rom disabled: {}", val);
                    self.boot_rom_disabled = true;
                },

                0xFFFF | 0xFF0F => {
                    if addr == 0xFF0F  {
                        //println!("IF wrote: {:02X}", val);
                    }
                    self.interrupts.write8(addr, val);
                },
                _ => {
                    self.mem[addr as usize] = val;
                }
            }
            return;
        };

    }

    pub fn tick(&mut self) {
        self.timer.tick();

        // Ppu runs 4 clock cycle for every cpu clock cycles
        for _ in 0..4 {
            self.ppu.tick();
        }
    }

    pub fn set_input(&mut self, input: &InputButtons) {
        self.prev_input = self.input.clone();
        let mut new_input = input.clone();
        new_input.not_select_action = self.prev_input.not_select_action;
        new_input.not_select_direction = self.prev_input.not_select_direction;
        self.input = new_input;
        
        if !self.prev_input.key_a && self.input.key_a ||
            !self.prev_input.key_b && self.input.key_b ||
                !self.prev_input.key_select && self.input.key_select ||
                !self.prev_input.key_start && self.input.key_start ||
                !self.prev_input.key_up && self.input.key_up ||
                !self.prev_input.key_down && self.input.key_down ||
                !self.prev_input.key_left && self.input.key_left ||
                !self.prev_input.key_right && self.input.key_right {
                    self.interrupts.interrupt_flag = self.interrupts.interrupt_flag | (1 << 4);
                }
    }

    // Display memory address. All addresses go through the BUS
    // NOTE: If we later change bus to tick on a read or write then we'll need 
    // to update this routine and access bus via anotiher method that does
    // not cause cycle ticks.
    pub fn hexdump(&self, start_addr: u16, len: u16) -> String {
        let mut output = String::new();
        let words_per_line  = 8;
        for i in 0..len/2 {
            if i*2 + 1 > len {
                break;
            }
            if i % words_per_line == 0 {
                output = output + &format!("{:04X}:", start_addr.wrapping_add((i as u16)*2));
            }
            let word;
            word = ((self.read8(start_addr.wrapping_add(2*i)) as u16) << 8) | (self.read8(start_addr.wrapping_add(2*i + 1)) as u16); 
            output = output + &format!(" {:04X}", word);
            if ((i+1) % words_per_line) == 0 {
                output = output + "\n";
            }
        }
        output
    }
}

/* Timer Register.
 * GB master clock speed is 4194304 4.2 MHz 
 * This is the master clock. 
 */
pub struct Timer {
    // HW Registers
    pub div: u16, // 0xFF04 - DIV: Divider Register, increments 16384 Hz. 
                    //          Resets when STOP instruction. Continues ticking when STOP ends
                    //          Does not cause interrupt
                    //          NOTE: For GBC increments at 32768 Hz
                    //          This is implemented as a u16 but the register is a u8. The u8
                    //          portion is the upper byte. The whole u16 register increments at
                    //
                    //          See PanDoc's timer schematic.
                    //          When bit 7 goes from 1 to 0 that means the 16384 Hz (tac=0b11)  timer has
                    //          ticked. Since 4.2Mhz/256 = 16384
                    //          When bit 3 goes from 1 to 0 that means the 262144 Hz (tac=0b01)
                    //          timer has ticked. Since 4.2 Mhz/16 = 252144
                    //          When bit 5 goes from 1 to 0 that means the 65536 Hz (tac=0b10) 
                    //          Whe bit 9 goes from 1 to 0 that means the 4096 Hz (tac=0b00). Since
                    //          4.2Mhz/1024 = 4096 

    pub tima: u8, // 0xFF05 - TIMA: Timer Counter Register
                    //          Increments at rate specified by TAC (Timer control).
                    //          After overflow, goes back to value specified by TMA, 
                    //          interrupt is then raised.
                    //           
                    //          
                    //
    pub tma: u8,  // 0xFF06 - TMA: Timer modulo 
                 //             When overflow occurs, TIMA is reset to this.
                 //             If TMA write occurs the same clock cycle as TIMA overflow,
                 //             then the old value of TMA is used for that write.
    pub tac: u8,  // 0xFF07 - TAC: Timer Control Register
                 //              Bit 2 - Timer enabled, 
                 //              Bit 1-0. 
                 //              0b00 = 4096 Hz, 0b01 = 262144 Hz, 
                 //              0b10 = 65536 Hz, 0b11 = 16384 Hz
                 //

    // Implementation details
    pub prev_clock_overflow: bool,
    pub tima_overflowed: bool,
    pub tima_reloaded: bool,


    pub interrupts: Shared<Interrupts>,
}

pub struct Interrupts {
    pub interrupt_enable_reg: u8,       // 0xFFFF  Interrupt Enable
                                        // Bit Number               ISR Handler addresss 
                                        // Bit 0 = VBlank           0x40 
                                        // Bit 1 = LCD STACK        0x48
                                        // Bit 2 = Timer            0x50
                                        // Bit 3 = Serial           0x58
                                        // Bit 4 = Joypad           0x60

    pub interrupt_flag: u8,             // 0xFF0F  Interrupt Flag
                                        // Interrupt flag. Set for device when interrupt is
                                        // requested. Interrupt actually only occurs if IME is
                                        // enabled
    
    pub ime: bool,              // IME - Interrupt Enable. 
                                // EI enables it, DI disables it
                                // RETI enables it
                                // ISR entry disables it

    pub prev_ime: bool,         // Used to implement the EI 1 cycle delay
}

impl Default for Interrupts {
    fn default() -> Self {
        Interrupts {
            interrupt_enable_reg: 0x0,
            interrupt_flag: 0x0,
            ime: false,
            prev_ime: false,
        }
    }
}
impl Interrupts {
    pub const INT_TIMER: u16 = 2;
    pub const INT_LCDSTAT: u16 = 1;
    pub const INT_VBLANK: u16 = 0;

    pub fn write8(&mut self, addr: u16, val: u8) {
        match addr {
            0xFFFF => {
                self.interrupt_enable_reg = val;
            },
            0xFF0F => {
                self.interrupt_flag = val;
            }
            _ => {
                panic!("attempt to read unknown interrupt reg: {:04X}", addr);
            }
        };
    }

    pub fn read8(&self, addr: u16) -> u8 {
        match addr {
            0xFFFF => {
                self.interrupt_enable_reg
            },
            0xFF0F => {
                self.interrupt_flag
            }
            _ => {
                panic!("attempt to read unknown interrupt reg: {:04X}", addr);
            }
        }
    }

    pub fn set_int(&mut self, int_num: u16) {
        self.interrupt_flag = self.interrupt_flag | (1 << int_num);
    }
}

impl Timer {
    // The timer runs at  4.2 Mhz master clock speed. The GB cpu runs at 1 Mhz. 
    // Since the cpu will be driving the tick's for all operations. 1 tick of the cpu will be 4
    // ticks of the timer. This is why we increment div by 4 every tick.
    const TIMER_TICK_PER_CPU_TICK: u16 = 4;

    pub fn new(interrupts: Shared<Interrupts>) -> Self {
        Timer {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            prev_clock_overflow: false,
            tima_overflowed: false,
            tima_reloaded: false,
            interrupts: interrupts.clone(),
        }
    }
    fn tick(&mut self) { 
        self.div = self.div.wrapping_add(Timer::TIMER_TICK_PER_CPU_TICK);

        //println!("tima: {}, self.div: {}", self.tima, self.div);
        self.tima_reloaded = false;

        // From Pandocs. When TIMA overflows, it does not load TMA immediately, there is a cycle
        // delay.  This handles that
        if self.tima_overflowed {
            // Now load TMA
            self.tima = self.tma;
            self.interrupts.interrupt_flag = self.interrupts.interrupt_flag | ((1 << Interrupts::INT_TIMER) as u8);
            self.tima_overflowed = false; 
            self.tima_reloaded = true;
        }

       let clock_select = self.tac & 0x3;
       let clock_overflow_bits: u8 = [9, 3, 5, 7][clock_select as usize];
       let cur_timer_overflow_bit = ((self.div >> clock_overflow_bits) & 0x1) == 1;
       let timer_enabled = (self.tac >> 2) & 1 == 1;
       // if the right timer bit was previously on and now its off then we overflowed the timer
       if self.prev_clock_overflow && !(cur_timer_overflow_bit && timer_enabled) {
           let (new_tima, overflowed) = self.tima.overflowing_add(1);
           //println!("tima tick: {}", self.tima);
           self.tima = new_tima;
           if overflowed {
               //println!("timer overflowed");
               self.tima_overflowed = true;
           }
       }
       self.prev_clock_overflow = cur_timer_overflow_bit && timer_enabled;
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        match addr {
            // DIV
            0xFF04 => {
                self.div = (val as u16)<< 8;
            },

            // TIMA
            0xFF05 => {
                /* From Pandocs:
                 * During the strange cycle [A] you can prevent the IF flag from 
                 * being set and prevent the TIMA from being reloaded from TMA by writing a 
                 * value to TIMA. That new value will be the one that stays in the 
                 * TIMA register after the instruction. Writing to DIV, TAC or other 
                 * registers wont prevent the IF flag from being set or TIMA from 
                 * being reloaded.
                 */
                
                //Writing to TIMA will cancel TIMA resetting into TMA.
                if self.tima_overflowed { 
                    self.tima_overflowed = false;
                }

                // From Pandoc's. A write to TIMA during TMA reloaded, write will be ignored
                if !self.tima_reloaded {
                    self.tima  = val;
                }
            },

            // TMA
            0xFF06 => {

                /* From Pandocs:
                 * If TMA is written the same cycle it is loaded to TIMA [B], TIMA is also loaded with that value.
                 */
                self.tma = val;
                if self.tima_reloaded {
                    self.tima = val;
                }
            },
            0xFF07 => {
                self.tac = val;
            },
            _ => {
                panic!("bad write to timer registers: {:4X}", addr);
            }
        }
    }

    pub fn read8(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => {
                (self.div >> 8) as u8
            },
            0xFF05 => {
                self.tima
            },
            0xFF06 => {
                self.tma 
            },
            0xFF07 => {
                self.tac
            },
            _ => {
                panic!("unknown timer read8: {:04X}", addr);
            }
        }
    }
}

// Gameboy Buttons
//
// Programs will set bit 5 or 4 then read bits 0-3
// Bit 7 - Not used
// Bit 6 - Not used
// Bit 5 - P15 Select Action buttons    (0=Select)
// Bit 4 - P14 Select Direction buttons (0=Select)
// Bit 3 - P13 Input: Down  or Start    (0=Pressed) (Read Only)
// Bit 2 - P12 Input: Up    or Select   (0=Pressed) (Read Only)
// Bit 1 - P11 Input: Left  or B        (0=Pressed) (Read Only)
// Bit 0 - P10 Input: Right or A        (0=Pressed) (Read Only)

#[derive(Default, Debug, Clone)]
pub struct InputButtons {

    pub key_up: bool,
    pub key_down: bool,
    pub key_left: bool,
    pub key_right: bool,
    pub key_a: bool,
    pub key_b: bool,
    pub key_start: bool,
    pub key_select: bool,

    // write-able
    pub not_select_action: bool,        // 0 = selected! thats why such a weird name
    pub not_select_direction: bool,     // ditto
}
