use crate::util::Shared;
use crate::bus::Interrupts;

// Gameboy is 160x144 resolution
const SCREEN_WIDTH_PX: u8 = 160;
const SCREEN_HEIGHT_PX: u8 = 144;

const DOTS_PER_LINE: u64 = 456;
const LINES_PER_FRAME: u8 = 154;
const SEARCH_OAM_DOT_END: u64 = 80;
const TRANSFER_OAM_DOT_END: u64 = 80 + 168;


const PX_PER_TILE: u64 = 8;
const TILE_MAP_WIDTH: u64 = 32;

// Each tile is 16 bytes. A tile is 8x8 px, the first two bytes are for first line from the top, second two are for
// second line, The first byte in each line contains the lsbits, the second byte contains the
// msbits
const BYTES_PER_TILE: u64 = 16;

// Gameboy is displayed  via 8x8 px tiles. Background layer is 32x32 tiles 
const SCREEN_WIDTH_TILE: u8 = 20; 
const SCREEN_HEIGHT_TILE: u8 = 18;



#[derive(PartialEq)]
pub enum PpuMode {
    HBlank = 0, // 
    VBlank = 1,
    SearchOAM = 2,
    Transfer = 3,
}

pub struct Ppu {
    // Registers
    pub lcdc: u8,                // 0xFF30 LCD Control
                            //      Bit 7 - LCD and PPU enable
                            //      Bit 6 - Window tile map area. 0 = 9800 - 9BFF , 1 = 9C00 - 9FFF
                            //      Bit 5 - Window enabe
                            //      Bit 4 - BG and Window Tile Data 0 = 8800- 97FF, 1 = 8000 -8FFFF
                            //      Bit 3 - BG Tile Map Area  0 = 9800 - 9BFF, 1 = 9C000 - 9FFFF
                            //      Bit 2 - OBJ Size 0 = 8x8, 1=8x16
                            //      Bit 1 - OBJ enable 
                            //      Bit 0 - BG and enable priority

    pub scy: u8,                // 0xFF42 SCY Viewport Y Position 
    pub scx: u8,                // 0xFF43 SCX Viewport X Position
    pub ly: u8,                 // 0xFF44 LY current line being drawn. 0 - 143 is screen. 144-153 is VBLANK
                            // period. 
    pub lyc: u8,                // 0xFF45 LYC. When LY is equal to LYC then the associated bit in LCD STAT
                            // register is set. And LCD STAT interrupt is raised.
    pub wy: u8,                 // 0xFF4A WY Window Y position
    pub wx: u8,                 // 0xFF4B WX Window X position. WX should be 7. Values 0 and 166
                            // unreliable. See Pan Docs

    pub tile_map: [u8; 0x800], // There are 2 tile map areas, 0x9800 - 9BFF, and 0x9C00 - 9FFFF
    pub tile_data: [u8; 0x1800], // 0x8000 - 0x9800 Tile Data memory

    // Implementation details
    pub interrupts: Shared<Interrupts>,
    pub lx: u64,        // Current dot for a line. There are 456 dots  in a scanline 
    pub lwy: u64,       // Current renderered window y
                    
    pub mode: PpuMode,       // Mode 0 = HBlank, 1 = VBlank, 2 = Search OAM, 3 = Transfer
                         //
                         
    pub line_buffer: Vec<u8>, // The current line we are rendering
}


impl Ppu {

    pub fn new(interrupts: Shared<Interrupts>) -> Ppu {
       Ppu {
           lcdc: 0,
           scy: 0,
           scx: 0,
           ly: 0,
           lyc: 0,
           wy: 0,
           wx: 0,
           interrupts: interrupts.clone(),
           lx: 0,
           lwy: 0,
           mode: PpuMode::SearchOAM,
           line_buffer: vec![0;  SCREEN_WIDTH_PX as usize],
           tile_map: [0; 0x800],
           tile_data: [0; 0x1800],
       }
    }

    pub fn do_transfer_of_line(&mut self) {
        // This is called during the transfer mode, i.e. we are rendering a line 
        
        // Clear our line buffer
        self.line_buffer.fill(0u8);

        // Render the bg or window
        self.do_transfer_of_bg_or_window();
        println!("line_buffer: {:?}", self.line_buffer);
        // Render the sprite layer
        self.do_transfer_of_sprites();

    }

    pub fn do_transfer_of_bg_or_window(&mut self) {

        // Loop through all the pixels in the current line from 0 to SCREEN_WIDTH_PX
        for x in 0u64..(SCREEN_WIDTH_PX as u64) {
            // Are we in the window range?
            let is_in_window_range = x + 7 >= (self.wx as u64) && self.ly >= self.wy;

            // Figure out the x and y coordinates of the current dot index
            // This will take into account the current SCX and SCY, if we are in the window
            // then don't use scx, use 7 + wx
            let scr_x = 
                if self.is_window_enabled() && is_in_window_range {
                    (x + 7 - (self.wx as u64)) as u64
                } else {
                    x + (self.scx as u64)
                };
            let scr_y: u64 = 
                if self.is_window_enabled() && is_in_window_range {
                    let render_window_y = self.lwy;
                    self.lwy += 1;
                    render_window_y
            } else {
                (self.ly.wrapping_add(self.scy)) as u64
            };
            // If we are inside of the window portion then we will display the window tile map
            // otherwise we only display the bg tilemap
             
            // From the x and y coordinate.
            // Find the tilemap that we are currently on
            let tile_map_x = scr_x / PX_PER_TILE; 
            let tile_map_y = scr_y / PX_PER_TILE;
            // Find the offset within the tilemap 
            let tile_offset_x = (scr_x % PX_PER_TILE) as usize;
            let tile_offset_y = (scr_y % PX_PER_TILE) as usize;


            // From the tile map x and tile map y, find the index for the tile data
            let mut tile_data_index = tile_map_x + tile_map_y * TILE_MAP_WIDTH;

            // Adjust the tile map index based whether we are at the window area 
            // or bg area, and whether or not the window tile map area control bit 
            // or bg tile map area control bit is set
            tile_data_index += 
                if self.is_window_enabled() {
                    if self.is_window_tile_map_set() {
                        0x400
                    } else {
                        0x0
                    }
                } else {
                    if self.is_bg_tile_map_set() {
                        0x400
                    } else {
                        0x0
                    }
                };
            //println!("scr_x: {}, scr_y: {}, tile_data_index: {}", scr_x, scr_y, tile_data_index);
            // Handle 
            // From the tile data index, find the tile data for this x and y
            let tile_data_id: usize = self.tile_map[tile_data_index as usize] as usize;
            //println!("tile_data_id: {}", tile_data_id);

            // From the tile data id, find the offset into the tile data memory. Each tile is 16 bytes
            let mut tile_data_offset: usize  = tile_data_id * BYTES_PER_TILE as usize;


            // Adjust the tile_data_offset based on whether we are at the window area or bg area,
            // and whether or not the window tile data control bit or bg tile data control bit is
            // set
            tile_data_offset += 
                if self.is_bg_tile_data_set() {
                    0x0
                } else {
                    0x1000
                };
            if tile_data_offset >= 0x1800 {
                tile_data_offset -= 0x1000;
            }

            // Pluck correct 2 bits from the tilemap 
            // We extract the 2 bytes for the correct line from tile_data_offset
            let tile_lsbits = self.tile_data[tile_data_offset  + tile_offset_y*2 as usize]; 
            let tile_msbits = self.tile_data[tile_data_offset + tile_offset_y*2 + 1 as usize];

            // We use the offset x to determine which  2 bits to combine together to get the color index
            let color_bits = 
                (tile_lsbits >> (7 - tile_offset_x)) & 1 |         // lower bit
                (((tile_msbits >> (7 - tile_offset_x)) & 1) << 1);   // upper bit

            // From the color index we go into our palette and find the actual color
            let color = self.from_color_to_color_byte(color_bits);
            self.line_buffer[scr_x as usize] = color;
        }

    }

    pub fn do_transfer_of_sprites(&mut self) {
        // TODO: handle sprites
    }

    pub fn from_color_to_color_byte(&self, color_bits: u8) -> u8 {
        // TODO: actually do some kind of palette look up
        color_bits
    }


    pub fn is_ppu_enabled(&self) -> bool {
        (self.lcdc >> 7) & 1 == 1
    }

    pub fn is_window_enabled(&self) -> bool {
        (self.lcdc >> 5) & 1 == 1
    }

    pub fn is_window_tile_map_set(&self) -> bool {
        (self.lcdc >> 6) & 1 == 1
    }

    pub fn is_bg_tile_map_set(&self) -> bool {
        (self.lcdc >> 3) & 1 == 1
    }

    pub fn is_bg_tile_data_set(&self) -> bool {
        (self.lcdc >> 4) & 1 == 1
    }

    pub fn tick(&mut self) {
        self.lx += 1;
        // DOTS_PER_LINE is not the same as screen pixels per line
        if self.lx == DOTS_PER_LINE {
            self.lx = 0;
            self.ly += 1;
            if self.ly == LINES_PER_FRAME {
                self.ly = 0;
                self.lwy = 0;
                // The frame is done
            }
        }

        // Are we in the non-vblank period 
        if self.ly <= SCREEN_HEIGHT_PX {
            if self.lx  < SEARCH_OAM_DOT_END {
                if self.set_mode(PpuMode::SearchOAM) {
                }
            } else if self.lx < TRANSFER_OAM_DOT_END {
                if self.set_mode(PpuMode::Transfer) {
                    self.do_transfer_of_line();
                }
            } else {
               if self.set_mode(PpuMode::HBlank) {
                   // Set the Hblank interrupt
               }
            }
       } else {
           if self.set_mode(PpuMode::VBlank) {
               // Set the VBlank interrupt
           }
       }
    }

    pub fn set_mode(&mut self, new_mode: PpuMode) -> bool {
        if self.mode == new_mode {
            return false;
        }
        self.mode = new_mode;        
        true
    }
}


#[test]
fn test_ppu() {
    use crate::ppu::Ppu;
    use crate::util::Shared;
    use crate::bus::Interrupts;
    let interrupts = Shared::new(Interrupts::default());
    let mut ppu = Ppu::new(interrupts.clone());

    // fill bg tile data set with some tiles. Lets make the tile the following:
    // 0 1 2 3 0 1 2 3            Byte 0 and Byte 1         lsbits 01010101 msbits: 00110011
    // 1 2 3 0 1 2 3 0            Byte 2 and Byte 3         lsbits 10101010 msbits: 01100110
    // 2 3 0 1 2 3 0 1            Byte 4 and Byte 5         lsbits 01010101 msbits: 11001100
    // 3 0 1 2 3 0 1 2            Byte 6 and Byte 7         lsbits 10101010 msbits: 10011001
    // 0 1 2 3 0 1 2 3            Byte 8 and Byte 9         lsbits 01010101 msbits: 00110011 
    // 1 2 3 0 1 2 3 0            Byte 10 and Byte 11       lsbits 10101010 msbits: 01100110
    // 2 3 0 1 2 3 0 1            Byte 12 and Byte 13       lsbits 01010101 msbits: 11001100
    // 2 3 0 1 2 3 0 1            Byte 14 and Byte 15       lsbits 10101010 msbits: 10011001
    //
    // The above works out to tile data:
    // 0x55 0x33 0xAA 0x66 0x55 0xCC 0xAA 0x99 0x55 0x33 0xAA 0x66 0x55 0xCC 0xAA 0x99

    let tile: Vec<u8>= vec![0x55, 0x33, 
                    0xAA, 0x66,
    0x55, 0xCC,
    0xAA, 0x99,
    0x55, 0x33,
    0xAA, 0x66,
    0x55, 0xCC,
    0xAA, 0x99];

    // 20 tiles across 
    let many_tiles = &(&tile)
        .into_iter()
        .cycle()
        .take(16*20).cloned()
        .collect::<Vec<u8>>();
    ppu.tile_data[0..16].copy_from_slice(&tile[..]);
    //ppu.tile_data[0..16*20].copy_from_slice(&many_tiles[..]);


    // Set lcdc bit 4 so we use the normal tile_data 
    ppu.lcdc = 0x10;
    for _ in 0..DOTS_PER_LINE {
        ppu.tick();
    }

    for _ in 0..DOTS_PER_LINE {
        ppu.tick();
    }

    for _ in 0..DOTS_PER_LINE {
        ppu.tick();
    }

    for _ in 0..DOTS_PER_LINE {
        ppu.tick();
    }

}
