use crate::util::Shared;

pub enum MapperError {
    UnknownAddress(u16)
}

pub trait Mapper {
    fn read8(&self, addr: u16, boot_rom_disabled: bool) -> Option<u8>;
    fn write8(&mut self, addr: u16, val: u8) -> Result<(), MapperError>;
}

// TODO: This should be a overall wrapper for multiple MBCs, it will switch based on the MBC id
// register
pub struct Mbc {
    rom: Shared<Vec<u8>>,
    rom_select: u8,
    ram_enabled: bool,
    advanced_ram_rom_bank_enabled: bool,
    ram_bank_select_or_rom_bank_upper: u8,

    // RAM
    ram: Shared<Vec<u8>>
}

impl Mapper for Mbc {
    fn read8(&self, addr: u16, boot_rom_disabled: bool) -> Option<u8> {
        match addr {
            0x00..=0xFF => {
                if !boot_rom_disabled {
                    None
                } else {
                    // TODO: Handle use of advanced_ram_rom_bank_enabled
                    //println!("boot rom disabled addr: {:04X} val:{:02X}", addr, self.rom[addr as usize]);
                    Some(self.rom[addr as usize])
                }
            },

            0x0100..=0x3FFF => {
                // TODO: Handle use of advanced_ram_rom_bank_enabled
                Some(self.rom[addr as usize])
            },

            0x4000..=0x7FFF => {
                let offset = addr as u32 - 0x4000;
                // TODO: Check to see if rom_select is greater than MBC actually ROM size.
                // If it is clamp it.
                let rom_select = if self.rom_select == 0 { 1 } else { self.rom_select };
                let target_addr: u32 = 0x4000*(rom_select as u32) + offset;
                //println!("mapper translate address {:04X} -> {:08X}", addr, target_addr);
                Some(self.rom[target_addr as usize])
            },

            0xA000..=0xBFFF => {
                if self.ram_enabled {
                    let ram_offset = 
                        if self.advanced_ram_rom_bank_enabled {
                            self.ram_bank_select_or_rom_bank_upper as usize * 0x2000  + ((addr - 0xA000) as usize)
                         } else {
                             (addr - 0xA000) as usize
                         };
                    Some(self.ram[ram_offset])
                } else {
                    Some(0xFF)
                }
            },

            _ => {
                None
            }
        }
    }

    fn write8(&mut self, addr: u16, val: u8) -> Result<(), MapperError> {
        match addr {
            0x0000 ..=0x1FFF => {
                println!("mapper: RAM enable: addr: {:04X} val: {:02X}", addr, val);
                if val & 0xF == 0xA {
                    self.ram_enabled = true;
                } else {
                    self.ram_enabled = false;
                }
                Ok(())
            },

            0x2000..=0x3FFF => {
                //println!("mapper: select rom: {}", val);
                self.rom_select = val & 0x1F;
                Ok(())
            },

            0x4000 ..= 0x5FFF => {
                self.ram_bank_select_or_rom_bank_upper = val & 0x3;
                println!("mapper: ram bank select/rom bank upper: {:04X} val: {}", self.ram_bank_select_or_rom_bank_upper, val);
                Ok(())
            },

            0x6000 ..= 0x7FFF => {
                println!("mapper: ram banking mode select: {:04X} val: {}", addr, val);
                self.advanced_ram_rom_bank_enabled = val  == 1;
                Ok(())
            },

            0xA000..=0xBFFF => {
                if self.ram_enabled{
                    let ram_offset = 
                        if self.advanced_ram_rom_bank_enabled {
                            self.ram_bank_select_or_rom_bank_upper as usize * 0x2000  + ((addr - 0xA000) as usize)
                         } else {
                             (addr - 0xA000) as usize
                         };
                    self.ram[ram_offset] = val;
                    Ok(())
                } else {
                    //println!("write to ram: {:04X} ignored, ram is disabled", addr);
                    // TODO: Not sure if this is correct
                    self.ram[addr as usize - 0xA000] = val;
                    Ok(())
                }
            },

            _ => {
                Err(MapperError::UnknownAddress(addr))
            }
        }
    }
}

impl Mbc {
   pub fn new(gb_rom: Shared<Vec<u8>>, boot_rom: &[u8])  -> Self {
       let mut mbc = 
        Mbc {
            rom: gb_rom.clone(),
            rom_select: 0,
            ram_enabled: false,
            advanced_ram_rom_bank_enabled: false,
            ram_bank_select_or_rom_bank_upper: 0,
            ram: Shared::new(vec![0; 0x8000]), // 32K of RAM 
        };
       mbc
   }

   pub fn new_without_bootrom(gb_rom: Shared<Vec<u8>>)  -> Self {
        Mbc {
            rom: gb_rom.clone(),
            rom_select: 0,
            ram_enabled: false,
            advanced_ram_rom_bank_enabled: false,
            ram_bank_select_or_rom_bank_upper: 0,
            ram: Shared::new(vec![0; 0x8000]), // 32K of RAM
        }
   }
}



