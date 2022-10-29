use std::rc::Rc;
use std::cell::RefCell;

pub struct Bus {
    pub mem: Vec<u8>
}


impl Bus {
    pub fn new(mem: &[u8])  -> Self {
        Bus {
            mem: Vec::from(mem)
        }
    }

    pub fn read8(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        self.mem[addr as usize] = val;
    }
}
