use crate::util::Shared;
use crate::bus::Interrupts;

// Gameboy is 160x144 resolution
pub const SCREEN_WIDTH_PX: u8 = 160;
pub const SCREEN_HEIGHT_PX: u8 = 144;

const DOTS_PER_LINE: u64 = 456;
const LINES_PER_FRAME: u8 = 154;
const SEARCH_OAM_DOT_END: u64 = 80;
const TRANSFER_OAM_DOT_END: u64 = 80 + 168;
const NUM_SPRITES: usize = 40;

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
    pub lcdc: u8,               // 0xFF40 LCD Control
                                //      Bit 7 - LCD and PPU enable
                                //      Bit 6 - Window tile map area. 0 = 9800 - 9BFF , 1 = 9C00 - 9FFF
                                //      Bit 5 - Window enabe
                                //      Bit 4 - BG and Window Tile Data 0 = 8800- 97FF, 1 = 8000 -8FFFF
                                //      Bit 3 - BG Tile Map Area  0 = 9800 - 9BFF, 1 = 9C000 - 9FFFF
                                //      Bit 2 - OBJ Size 0 = 8x8, 1=8x16
                                //      Bit 1 - OBJ enable 
                                //      Bit 0 - BG and enable priority

    pub lcdstat: u8,            // 0xFF41 LCD Status 
                                
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

    pub sprite_attributes: [u8; 160], // The Sprite Attribute Tables at 0xFE00, 4 bytes per sprite
                                      // There are 40 entries. The HW makes use of 10 per scanline
                                      
    // Implementation details
    pub interrupts: Shared<Interrupts>,
    pub lx: u64,        // Current dot for a line. There are 456 dots  in a scanline 
    pub lwy: u64,       // Current renderered window y
                    
    pub mode: PpuMode,       // Mode 0 = HBlank, 1 = VBlank, 2 = Search OAM, 3 = Transfer
                         //
                         
    pub line_buffer: Vec<u8>, // The current line we are rendering

    pub frame_buffer: Vec<Vec<u8>>,
    // TODO: This is a total kludge to sync our EGUI line display with output of new line
    pub line_output_count: u32,
    pub frame_output_count: u32,

    pub bg_palette: [u8; 4],
    pub obj_palette0: [u8; 4],
    pub obj_palette1: [u8; 4],
}
#[derive(Debug)]
struct Sprite {
    pub y: u8,
    pub x: u8,
    pub tile_index: u8,
    pub attr: u8,
    pub oam_index: u8,
}

impl Ppu {

    pub fn new(interrupts: Shared<Interrupts>) -> Ppu {
       Ppu {
           lcdc: 0,
           lcdstat: 0,
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
           sprite_attributes: [0; 160],
           frame_buffer: vec![vec![0; SCREEN_WIDTH_PX as usize] ; SCREEN_HEIGHT_PX as usize],
           line_output_count: 0,
           frame_output_count:0,
           bg_palette: [0, 1, 2, 3],
           obj_palette0: [0, 1, 2, 3],
           obj_palette1: [0, 1, 2, 3],

       }
    }

    pub fn write8(&mut self, addr: u16, val: u8)  {
        match addr {
            0x8000..=0x97FF  => {
                //println!("Ppu::write8 tile_data : {:04X}, val: {:02X}", addr, val);
                self.tile_data[(addr - 0x8000) as usize] = val;
            },
            0x9800..= 0x9FFF => {
                //println!("Ppu::write8 tile_map: {:04X}, value: {:02X}", addr, val);
                self.tile_map[(addr - 0x9800) as usize] = val;
            },
            0xFE00..=0xFE9F => {
                //println!("Ppu::read8 sprite attribute: {:04X}, val: {:02X}", addr, val);
                self.sprite_attributes[(addr - 0xFE00) as usize] = val;
            },

            0xFF40 => {
                println!("Ppu LCDC write: {:02X}", val);
                self.lcdc = val;
            },

            0xFF41 => {
                self.lcdstat = val;
            },

            0xFF42 => { 
                self.scy = val;
            },

            0xFF43 => {
                self.scx = val;
            },
            0xFF45 => {
                self.lyc = val;
            },
            0xFF47 => {
                self.bg_palette[3] = val >> 6 & 0x3;
                self.bg_palette[2] = val >> 4 & 0x3;
                self.bg_palette[1] = val >> 2 & 0x3;
                self.bg_palette[0] = val & 0x3;
            },
            0xFF48 => {
                self.obj_palette0[3] = val >> 6 & 0x3;
                self.obj_palette0[2] = val >> 4 & 0x3;
                self.obj_palette0[1] = val >> 2 & 0x3;
                self.obj_palette0[0] = val & 0x3;
            },
            0xFF49 => {
                self.obj_palette1[3] = val >> 6 & 0x3;
                self.obj_palette1[2] = val >> 4 & 0x3;
                self.obj_palette1[1] = val >> 2 & 0x3;
                self.obj_palette1[0] = val & 0x3;
            },
            0xFF4A => {
                self.wy = val;
            },
            0xFF4B => {
                self.wx = val;
            },

            _ => { 
                panic!("Ppu::write8 unknown ppu address range: {:04X}, value: {:02X}", addr, val);
            }
        }
    }

    pub fn read8(&self, addr: u16) -> u8 {
        match addr {
            0x8000..=0x97FF => {
                //println!("Ppu::read8 tile_data : {:04X}", addr);
                self.tile_data[(addr - 0x8000) as usize]
            },

            0x9800..=0x9FFF => {
                //println!("Ppu::write8 tile_map: {:04X}", addr);
                self.tile_map[(addr - 0x9800) as usize]
            },

            0xFE00..=0xFE9F => {
                //println!("Ppu::read8 sprite attribute: {:04X}", addr);
                self.sprite_attributes[(addr - 0xFE00) as usize]
            },

            0xFF40 => {
                self.lcdc
            },

            0xFF41 => {
                self.lcdstat
            },

            0xFF42 => {
                self.scy
            },

            0xFF43 => {
                self.scx
            }

            0xFF44 => {
                self.ly
            },
            0xFF45 => {
                self.lyc
            },
            0xFF47 => {
                self.bg_palette[3] << 6 |
                self.bg_palette[2] << 4 |
                self.bg_palette[1] << 2 |
                self.bg_palette[0]
            },
            0xFF48 => {
                self.obj_palette0[3] << 6 |
                self.obj_palette0[2] << 4 |
                self.obj_palette0[1] << 2 |
                self.obj_palette0[0]
            },
            0xFF49 => {
                self.obj_palette1[3] << 6 |
                self.obj_palette1[2] << 4 |
                self.obj_palette1[1] << 2 |
                self.obj_palette1[0]
            },
            0xFF4A => {
                self.wy
            },
            0xFF4B => {
                self.wx
            },

            _ => {
                panic!("Ppu::read8 unkonwn ppu address range: {:04X}", addr);
            }
        }
    }

    pub fn write_oam_dma(&mut self, val: u8, src: &[u8]) {
        // The 0xFF46 register is the DMA register.
        // Writing $XX to it will start transfer of bytes $XX00 - $XX9F
        // to 0xFE00-0xFE9F.
        // This transfer takes 160 cycles
        //println!("Ppu::write_oam_dma: {:02X}", val);
        for i in 0x00..=0x9F {
            self.sprite_attributes[i as usize] = src[i];
        }
    }
    pub fn do_transfer_of_line(&mut self) {
        // This is called during the transfer mode, i.e. we are rendering a line 
        
        // Clear our line buffer
        self.line_buffer.fill(0u8);

        // Render the bg or window
        self.do_transfer_of_bg_or_window();
        //println!("line_buffer: {:?}", self.line_buffer);


        // Render the sprite layer
        self.do_transfer_of_sprites();


        // Copy the line to the frame buffer, converting from color palette index into actual rgb
        // color for display in GUI
        if self.ly < SCREEN_HEIGHT_PX {
            self.frame_buffer[self.ly as usize] = self.line_buffer.clone(); 
        }
        self.line_output_count = self.line_output_count.wrapping_add(1);
    }

    pub fn do_transfer_of_bg_or_window(&mut self) {

        let mut window_rendered = false;
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
                    window_rendered = true;
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
            //println!("x: {}, y: {} tx: {} ty: {}", scr_x, scr_y, tile_map_x, tile_map_y);
            //
            // Adjust the tile map index based whether we are at the window area 
            // or bg area, and whether or not the window tile map area control bit 
            // or bg tile map area control bit is set
            tile_data_index += 
                //if self.is_window_enabled() && is_in_window_range {
                if self.is_window_enabled() && is_in_window_range {
                    if self.is_window_tile_map_bit_set() {
                        0x400
                    } else {
                        0x0
                    }
                } else {
                    if self.is_bg_tile_map_bit_set() {
                        0x400
                    } else {
                        0x0
                    }
                };
            //println!("scr_x: {}, scr_y: {}, tile_map_x: {} tile_map_y: {} tile_data_index: {}", scr_x, scr_y, tile_map_x, tile_map_y, tile_data_index);
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
                if self.is_bg_tile_data_bit_set() {
                    0x0
                } else {
                    0x1000
                };
            if tile_data_offset >= 0x1800 {
                tile_data_offset -= 0x1000;
            }

            // We extract the 2 bytes for the correct line from tile_data_offset. First byte 
            // contains the lsbits of the first row of tile, second byte contains the msbits 
            // of the first row of file.
            // The third byte and 4th btyte contain the second row of the tile, and so on.
            // TODO: Need to handle bg attribute of flipping y. In this case the first and second byte is the
            // last row of the tile.
            let tile_lsbits = self.tile_data[tile_data_offset  + tile_offset_y*2 as usize]; 
            let tile_msbits = self.tile_data[tile_data_offset + tile_offset_y*2 + 1 as usize];

            // We use the offset x to determine which  2 bits to combine together to get the color index
            let mut color_bits = 
                (tile_lsbits >> (7 - tile_offset_x)) & 1 |         // lower bit
                (((tile_msbits >> (7 - tile_offset_x)) & 1) << 1);   // upper bit

            color_bits = self.bg_palette[color_bits as usize];
            // From the color index we go into our palette and find the actual color
            let color = self.from_color_to_color_byte(color_bits);
            self.line_buffer[x as usize] = color;
        }
        if window_rendered {
            self.lwy += 1;
        }
    }

    pub fn do_transfer_of_sprites(&mut self) {
        // TODO: handle sprites
        
        if !self.is_sprite_enabled() {
            return
        }
        // Sprite attributes are in 0xFE00 - 0xFE9F
        // There are 40 sprites taking up 160 bytes
        // It consists of:
        // offset           value
        // 0                y position
        // 1                x position
        // 2                tile data id
        // 3                attributes
        //                      7 - BG/Win over sprite
        //                      6 - y flip
        //                      5 - x flip
        //                      4 - palette  *non color gameboy only*
        //                      3 - tile vram bank *color gameboy only*
        //                      2-0 - palette *color gameboy only*

        // Sprite's Y position is really position - 16
        // Sprite's X position is really position - 8

        // Ppu only displays 10 sprites on a line. Any sprites after that are
        // ignored

        // Find the first sprites, up to 10 that will be drawn on this line
        // FIXME?: Should we move this scan to inside the main loop below? 
        // This is due to maybe the program changing between 8x8 and 8x16 tiles midway
        let mut candidate_sprites = vec![]; 
        for i in 0..NUM_SPRITES {
            let sprite_y = self.sprite_attributes[i * 4 as usize];
            // TODO: Is this a bug if they change sprite tile mode mid way through line?
            if self.ly >= sprite_y - 16 && self.ly < (if self.is_sprite_8x16_mode() { sprite_y } else { sprite_y - 8}) {
                let sprite = Sprite {
                    y: self.sprite_attributes[i*4],
                    x: self.sprite_attributes[i*4 + 1],  
                    tile_index: self.sprite_attributes[i*4 + 2],
                    attr: self.sprite_attributes[i*4 + 3],
                    oam_index: i as u8,
                };
                candidate_sprites.push(sprite);
            }
            if candidate_sprites.len() == 10 {
                break;
            }
        }

        if candidate_sprites.len() == 0 {
            return;
        } else {
            candidate_sprites.sort_by(|s1, s2| 
                                      if s1.x == s2.x {
                                          s1.oam_index.cmp(&s2.oam_index)
                                      } else {
                                          s1.x.cmp(&s2.x)
                                      });
        }
        // draw the line with any sprites on it  by walking down the line
        // and extracing the sprite tile at that position
        for x in 0..SCREEN_WIDTH_PX {

            // loop through candidate sprites and determine which one is on this x position
            for s in &candidate_sprites {
                //println!("sprite: {:?}", s);
                // is the sprite covering this position?
                if x >= s.x - 8 && x < s.x {
              
                    let offset_x = x - (s.x - 8);
                    let offset_y = self.ly - (s.y - 16);
                    if !self.is_sprite_8x16_mode() && offset_y >= 8 {
                        panic!("we are in 8x8 sprite mode but got y offset > 8. sprite:{:?}", s);
                    }

                    let lsbits; 
                    let msbits;
                    // from the y offset we get what bytes of the tile to grab
                    // TODO: handle flipped y
                    if offset_y >= 8 {
                        // get tile from second 8x8 tile
                        let offset_y: usize = offset_y as usize - 8;
                        lsbits = self.tile_data[s.tile_index as usize * 16 + offset_y*2 + 2];
                        msbits = self.tile_data[s.tile_index as usize * 16 + offset_y*2 + 3];
                    } else {
                        // normal
                        let offset_y= offset_y as usize;
                        lsbits = self.tile_data[s.tile_index as usize * 16 + offset_y*2 ];
                        msbits = self.tile_data[s.tile_index as usize * 16 + offset_y*2 + 1];
                    }

                    // from the lsbits and msbits we draw the pixel for that row
                    // if the sprite's color is 0 it is transparent, don't draw it
                    // TODO: handle flip X 
                    let mut color_bits = 
                        (lsbits >> (7 - offset_x)) & 1 |         // lower bit
                        (((msbits >> (7 - offset_x)) & 1) << 1);   // upper bit

                    // From the color index we go into our palette and find the actual color
                    if color_bits != 0 {
                        let palette0 = s.attr >> 4 & 1 == 0;
                        if palette0 {
                            color_bits = self.obj_palette0[color_bits as usize];
                        } else {
                            color_bits = self.obj_palette1[color_bits as usize];
                        }
                        let color = self.from_color_to_color_byte(color_bits);
                        self.line_buffer[x as usize] = color;


                        // TODO: I think we should probably process all sprites 
                        // so that 2 sprites can cover the same pixel if one sprite is transparent.
                        break;
                    }
                }
            }

        }
    }

    pub fn is_sprite_8x16_mode(&self) -> bool {
        (self.lcdc  >> 2) & 1 == 1 
    }

    pub fn is_sprite_enabled(&self) -> bool {
        (self.lcdc >> 1) & 1 == 1
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

    pub fn is_window_tile_map_bit_set(&self) -> bool {
        (self.lcdc >> 6) & 1 == 1
    }

    pub fn is_bg_tile_map_bit_set(&self) -> bool {
        (self.lcdc >> 3) & 1 == 1
    }

    pub fn is_bg_tile_data_bit_set(&self) -> bool {
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


            // TODO: Check LY = LYC and set LCDSTAT bit
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
               self.interrupts.set_int(Interrupts::INT_VBLANK);
               self.frame_output_count = self.frame_output_count.wrapping_add(1);
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



    // Helper routine to display tiles 
    pub fn get_tile_as_vec(&self, idx: u16) -> Vec<Vec<u8>> {
        // Given a tile index grab the 2 bytes and return a 8x8  vec of the tile with color index
        let tile_data_idx = idx as usize *  BYTES_PER_TILE as usize; // 2 bytes per tile

        let mut tile: Vec<Vec<u8>> = vec![];
        for y in 0..8 {
            let mut row = vec![0; 8];
            let lsbits = self.tile_data[(tile_data_idx + 2*y) as usize];
            let msbits = self.tile_data[(tile_data_idx + 2*y+1) as usize];
            for x in 0..8 {
                let color = ((lsbits >> (7-x)) & 1) | ((msbits >> (7-x)) & 1) << 1;
                row[x] = color;
            }
            tile.push(row);
        }
        tile
    }
}


#[ignore]
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
