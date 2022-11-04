use std::rc::Rc;
use std::cell::RefCell;

pub struct Bus {
    pub mem: Vec<u8>
}


impl Bus {
    pub fn new(mem: &[u8])  -> Self {
        let mut bus =     Bus {
            mem: vec![0x0; 0x20000],
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
        self.mem[addr as usize] = val;
    }
}
