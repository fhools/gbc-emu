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
    scy: u8,             // 0xFF42 SCY Viewport Y Position 
    scx: u8,             // 0xFF43 SCX Viewport X Position
    ly: u8,              // 0xFF44 LY current line being drawn. 0 - 143 is screen. 144-153 is VBLANK
                         // period. 
    lyc: u8,             // 0xFF45 LYC. When LY is equal to LYC then the associated bit in LCD STAT
                         // register is set. And LCD STAT interrupt is raised.
    wy: u8,              // 0xFF4A WY Window Y position
    wx: u8,              // 0xFF4B WX Window X position. WX should be 7. Values 0 and 166
                         // unreliable. See Pan Docs

    tile_map: [u8; 0x800], // There are 2 tile map areas, 0x9800 - 9BFF, and 0x9C00 - 9FFFF
    tile_data: [u8; 0x1800], // 0x8000 - 0x9800 Tile Data memory

    // Implementation details
    interrupts: Shared<Interrupts>,
    lx: u64,        // Current dot for a line. There are 456 dots  in a scanline 
                    //
    mode: PpuMode,       // Mode 0 = HBlank, 1 = VBlank, 2 = Search OAM, 3 = Transfer
                         //
                         
    line_buffer: Vec<u8>, // The current line we are rendering
}


impl Ppu {

    pub fn new(interrupts: Shared<Interrupts>) -> Ppu {
       Ppu {
           scy: 0,
           scx: 0,
           ly: 0,
           lyc: 0,
           wy: 0,
           wx: 0,
           interrupts: interrupts.clone(),
           lx: 0,
           mode: PpuMode::SearchOAM,
           line_buffer: vec![0;  SCREEN_WIDTH_PX as usize],
           tile_map: [0; 0x800],
           tile_data: [0; 0x1800],
       }
    }

    pub fn do_transfer_of_line(&mut self) {
        // This is called during the transfer mode, i.e. we are rendering a line 
        
        // Clear our line buffer
        self.line_buffer.clear();


        // Render the bg or window
        self.do_transfer_of_bg_or_window();

        // Render the sprite layer
        self.do_transfer_of_sprites();

    }

    pub fn do_transfer_of_bg_or_window(&mut self) {

        // TODO: This should actually be in a loop, this is just to flesh out the algorithm
        //
        // Loop through all the pixels in the current line from 0 to SCREEN_WIDTH_PX
        let scr_x = 100;
        let scr_y = 80;
        // Figure out the x and y coordinates of the current dot index
        // This will take into account the current SCX and SCY
        // If we are inside of the window portion then we will display the window tile map
        // otherwise we only display the bg tilemap
        //
        // From the x and y coordinate.
        // Find the tilemap that we are currently on
        let tile_map_x = scr_x / PX_PER_TILE; 
        let tile_map_y = scr_y / PX_PER_TILE;
        // Find the offset within the tilemap 
        let tile_offset_x = (scr_x % PX_PER_TILE) as usize;
        let tile_offset_y = (scr_y % PX_PER_TILE) as usize;


        // From the tile map x and tile map y, find the index for the tile data
        let tile_data_index = tile_map_x + tile_map_y * TILE_MAP_WIDTH;

        // From the tile data index, find the tile data for this x and y
        let tile_data_id: usize = self.tile_map[tile_data_index as usize] as usize;


        // From the tile data id, find the offset into the tile data memory. Each tile is 16 bytes
        // TODO: we need to address tile_data_offset based on LCDC Bit 4.
        let tile_data_offset: usize  = tile_data_id * BYTES_PER_TILE as usize;

        // Pluck correct 2 bits from the tilemap 
        // Each tile has 2 bytes per line, where the first byte is the lsbits, 
        // and second byte is the msbits


        // We extract the 2 bytes for the correct line from tile_data_offset
        let tile_lsbits = self.tile_data[tile_data_offset  + tile_offset_y*2 as usize]; 
        let tile_msbits = self.tile_data[tile_data_offset + tile_offset_y*2 + 1 as usize];

        // We use the offset x to determine which bits to combine together to get the color index
        let color_bits = 
            (tile_lsbits >> (7 - tile_offset_x)) & 1 |         // lower bit
            (((tile_msbits >> (7 - tile_offset_x)) & 1) << 1);   // upper bit

        // From the color index we go into our palette and find the actual color
        let color = self.from_color_to_color_byte(color_bits);
        self.line_buffer[scr_x as usize] = color;
        
    }

    pub fn do_transfer_of_sprites(&mut self) {
        // TODO: handle sprites
    }

    pub fn from_color_to_color_byte(&self, color_bits: u8) -> u8 {
        // TODO: actually do some kind of palette look up
        color_bits
    }
    pub fn tick(&mut self) {
        self.lx += 1;
        // DOTS_PER_LINE is not the same as screen pixels per line
        if self.lx == DOTS_PER_LINE {
            self.lx = 0;
            self.ly += 1;
            if self.ly == LINES_PER_FRAME {
                self.ly = 0;
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
                   // Draw the line
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
