use std::rc::Rc;
use std::cell::RefCell;

pub struct Bus {
    pub mem: Vec<u8>,
    pub timer: Timer
}


impl Bus {
    pub fn new(mem: &[u8])  -> Self {
        let mut bus =     Bus {
            mem: vec![0x0; 0x20000],
            timer: Default::default(),
        };
        // note: when i did bus.copy_from_slice, got panic that source len != dest len
        // so had to transform dest into a slice first
        bus.mem[0..mem.len()].copy_from_slice(&Vec::from(mem)[..]);
        bus
    }

    pub fn read8(&self, addr: u16) -> u8 {
        // TODO: demultiplex addr to the varios IO ranges
        self.mem[addr as usize]
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        // TODO: demmultiplexeer addr to various IO ranges

        match addr {
            0xFF04..=0xFF07 => {
                self.timer.write8(addr, val);
            },
            _ => {
                self.mem[addr as usize] = val;
            }
        }
    }

    pub fn tick(&mut self) {
        self.timer.tick();
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
    pub prev_clock_overflow_bit: bool,
    pub tima_overflowed: bool,
    pub tima_reloaded: bool,
}


impl Default for Timer {
    fn default() -> Self {
        Timer {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            prev_clock_overflow_bit: false,
            tima_overflowed: false,
            tima_reloaded: false,
        }
    }

}

impl Timer {
    // The timer runs at  4.2 Mhz master clock speed. The GB cpu runs at 1 Mhz. 
    // Since the cpu will be driving the tick's for all operations. 1 tick of the cpu will be 4
    // ticks of the timer. This is why we increment div by 4 every tick.
    const TIMER_TICK_PER_CPU_TICK: u16 = 4;

    fn tick(&mut self) {
       self.div = self.div.wrapping_add(Timer::TIMER_TICK_PER_CPU_TICK);

        self.tima_reloaded = false;

       // From Pandocs. When TIMA overflows, it does not load TMA immediately, there is a cycle
       // delay.  This handles that
       if self.tima_overflowed {
           // Now load TMA
            self.tima = self.tma;

            // TODO: set interrupt flag

           self.tima_overflowed = false; 
           self.tima_reloaded = true;
       }

       let clock_select = self.tac & 0x3;
       let clock_overflow_bits: u8 = [9, 3, 5, 7][clock_select as usize];
       let cur_timer_overflow_bit = (self.div >> clock_overflow_bits & 0x1) == 1;
       if self.prev_clock_overflow_bit && !cur_timer_overflow_bit {
           let (new_tima, overflowed) = self.tima.overflowing_add(1);
           self.tima = new_tima;
           if overflowed {
               self.tima_overflowed = true;
           }
       }
       self.prev_clock_overflow_bit = cur_timer_overflow_bit;
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
}
