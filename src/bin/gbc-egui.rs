use gbc_emu::cpu::{self, LR35902Cpu};
use gbc_emu::bus::{self, Bus, Interrupts};
use gbc_emu::ppu::{self, Ppu};
use gbc_emu::rom::read_rom;
use gbc_emu::util::Shared;
use gbc_emu::bootrom::BOOT_ROM;
use eframe::egui;
use egui::*;
use std::time::Duration;
use std::time::SystemTime;

struct GbcApp {
    cpu: cpu::LR35902Cpu,
    bus: Shared<bus::Bus>,
    run_continuous: bool,
    addr_to_dump: String,
    cycles: u64,
    is_gb_screen_open: bool,
    frame_output_count: u32,
}

impl GbcApp {
    const WIDTH: f32 = 160.0;
    const HEIGHT: f32 = 144.0;
    const COLORS: [color::Color32; 4] = [ color::Color32::from_rgb(255, 255, 255),
                                          color::Color32::from_rgb(170, 170, 170), 
                                          color::Color32::from_rgb(85, 85, 85),
                                          color::Color32::from_rgb(0, 0, 0)];

    fn screen_ui(&mut self, ui: &mut egui::Ui) {
        let j: i32 = self.bus.ppu.ly as i32;
        // allocate space in the child window
        let (response, painter) = ui.allocate_painter(Vec2::new(320.0, 288.0), Sense::hover());

        let gb_screen_size = Vec2::new(160.0, 144.0);
        // create a transform to go from local space to global window space
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, gb_screen_size),
            response.rect,
            );

        // output all the pixels, i'm using rects because i dont know any better way. seems kludgy
        for j in 0..(144 as i32) {
            for i in 0..(160 as i32) {
                // output a rect that represents a pixel, coordinates are local to the window
                let rect = Rect::from_min_max([i as f32, j as f32].into(), [(i+1) as f32, (j+1) as f32].into());
                let dotcolor = Self::COLORS[self.bus.ppu.frame_buffer[j as usize][i as usize] as usize];
                // draw the pixel, output to the window, painter wants global coordinates so use
                // to_screen.tranform_rect
                painter.add(egui::Shape::rect_filled(to_screen.transform_rect(rect), 
                                                     0.0, 
                                                     dotcolor));
            }
        }
    }


    fn tile_data_screen_ui(&mut self, ui: &mut egui::Ui) {
        let (response, painter) = ui.allocate_painter(Vec2::new(256.0, 384.0), Sense::hover());

        let gb_screen_size = Vec2::new(128.0, 192.0);
        // create a transform to go from local space to global window space
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, gb_screen_size),
            response.rect,
            );

        for j in 0..24 {
            for i in 0..16 {
                let tile = self.bus.ppu.get_tile_as_vec(j*16 + i);
                for (y,tile_row) in tile.into_iter().enumerate() {
                    for (x, color) in tile_row.into_iter().enumerate()  {
                        // output a rect that represents a pixel, coordinates are local to the window
                        let rect = Rect::from_min_max([(i*8 + (x as u16)) as f32, (j*8 + (y as u16)) as f32].into(), [((i*8 + (x as u16)) + 1) as f32, ((j*8 + (y as u16))  + 1) as f32].into());
                        let dotcolor = Self::COLORS[color as usize];
                        // draw the pixel, output to the window, painter wants global coordinates so use
                        // to_screen.tranform_rect
                        painter.add(egui::Shape::rect_filled(to_screen.transform_rect(rect), 
                                                             0.0, 
                                                             dotcolor));
                    }
                }
            }
        }
    }

    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        setup_custom_fonts(&cc.egui_ctx);


        // Setup a small program to test interrupts
        let main_program = [0x00, // nop
        0x31,                      // ld sp, nn  sp = 0xDFFF
        0xFF,
        0xDF,
        0x3E,    // ld a, n -- n = 0x0F
        0x0F,    //
        0xEA,    // ld (nn), a -- nn = 0xFFFF Interrupt Enable  
        0xFF,
        0xFF,
        0x3E,    // ld a, n -- n = 0xFF set 0xFF06 = 0xFE
        0xFE,    //
        0xEA,    // ld (nn), a -- nn = 0xFF06  TMA
        0x06,
        0xFF,
        0x3E,    // ld a, n -- n = 0xFE set 0xFF05 = 0xFE
        0xFE,    //
        0xEA,    // ld (nn), a -- nn = 0xFF05  TMA
        0x05,
        0xFF,
        0xFB,    // ei
        0x03,    // inc bc
        0x24,    // inc h
        0x24,    // inc h
        0x09,    // add hl, bc
        0x01,    // ld bc, nn (where nn = 0xbabe)
        0xbe,    // z80 is little endien so 0xbabe is 0xbe 0xba
        0xba,
        0x25,
        0xCB,    // rlc b 
        0x00,
        0xC3,    // jp 0x0000 ; go back to beginning
        0x00,
        0x00,
        ];
        
    let prog_ld_highram  = [0x00, // nop
                        0x3E, // ld a, $FE
                        0xFE,
                        0xEA, // ld ($FF90), a
                        0x90,
                        0xFF,
                        0x3E, 
                        0xDC, // ld a, $DC
                        0xEA, // ld $(FF91), a
                        0x91,
                        0xFF,
                        0x3E, // ld a, $BA
                        0xBA, 
                        0xEA, // ld $(FF92), a
                        0x92,
                        0xFF,
                        0xCE, // jp $0000
                        0x00,
                        0x00,
                       0x00,
    ]; 

    let prog_ldh  = [ 0x00, //nop
                    0x3E, // ld a, $DC
                    0xDC, 
                    0x0E, // ld c, $90
                    0x90,
                    0xEA, // ld $(FF91),a
                    0x91,
                    0xFF,
                    0xF0, // ldh a ($91)
                    0x91,
                    0xE0, // ldh ($91), a
                    0x91,
                    0xF2, // ld a, (c)
                    0xE2, // ld (c), a
                    0xFA, // ld a, ($FF91)
                    0x91,
                    0xFF,
                    0xEA, // ld ($FF91), a
                    0x91,
                    0xFF,
                    0x08,  // ld ($FF91), sp
                    0x91,
                    0xFF,
                    0x01,  // ld bc, $0123
                    0x23,
                    0x01,
                    0x11, // ld de, $0123
                    0x23,
                    0x01,
                    0x21, // ld hl, $0123
                    0x23,
                    0x01,
                    0x31, // ld sp, $0123
                    0x23,
                    0x01,
                    0xF5, // push af
                    0xC5, // push bc 
                    0xD5, // push de
                    0xE5, // push hl
        ];
        // The timer ISR to run
        // FIXME: Wrong ISR address its at 0x0050
        let isr_FF50 = [
            0x03, // inc h
            0xD9, // RETI
        ];

        let mut code_buffer = vec![0u8; 0xFFFF];
        //code_buffer[0..main_program.len()].copy_from_slice(&main_program[..]);
        code_buffer[0..prog_ldh.len()].copy_from_slice(&prog_ldh[..]);
        //code_buffer[0xFF50..0xFF50 + 2].copy_from_slice(&isr_FF50[..]);

        //let rom = read_rom("roms/mts-20221022/acceptance/ei_sequence.gb").unwrap();
        //let rom = read_rom("roms/mts-20221022/emulator-only/mbc1/bits_bank2.gb").unwrap();
        let rom = read_rom("roms/cpu_instrs.gb").unwrap();
        
        // TODO: Definately not going to pass mem_timing right now...
        // This is due to us ticking the bus/timer at the end of the cpu instruction execution.
        // We should tick the bus after does its load and store cycles
        //let rom = read_rom("roms/mem_timing.gb").unwrap();
        //let rom = read_rom("roms/01-special.gb").unwrap();
        //let rom = read_rom("roms/02-interrupts.gb").unwrap();
        //let rom = read_rom("roms/03-op-sh-hl.gb").unwrap();
        //let rom = read_rom("roms/04-op-r-imm.gb").unwrap();
        //let rom = read_rom("roms/05-op-rp.gb").unwrap();
        //let rom = read_rom("roms/06-ld-r-r.gb").unwrap();
        //let rom = read_rom("roms/07-jr-jp-call-ret-rst.gb").unwrap();
        //let rom = read_rom("roms/08-misc-instrs.gb").unwrap();
        //let rom = read_rom("roms/09-op-r-r.gb").unwrap();
        //let rom = read_rom("roms/10-bit-ops.gb").unwrap();
        //let rom = read_rom("roms/11-op-a-hl.gb").unwrap();
        println!("rom size: {}", rom.len());

        let rom = Shared::new(rom);
        let mem = Shared::new(vec![0u8; 0x90000]);
        let interrupts = Shared::new(Interrupts::default());
        let ppu = Shared::new(Ppu::new(interrupts.clone()));
        let bus = Shared::new(Bus::new(rom.clone(), &BOOT_ROM, mem.clone(), interrupts.clone(), ppu.clone()));
        //let bus = Shared::new(Bus::new_without_bootrom(rom.clone(), mem.clone(), interrupts.clone(), ppu.clone()));
        //let bus = Shared::new(Bus::new_without_bootrom(&code_buffer, mem.clone(), interrupts.clone(), ppu.clone()));
        // For ISR test program starts at 0x0
        //let cpu = LR35902Cpu::new(0x0, bus.clone());

        let cpu = LR35902Cpu::new(0x0, bus.clone());
   
        GbcApp {
            cpu,
            bus,
            run_continuous: false,
            addr_to_dump: String::new(),
            cycles: 0,
            is_gb_screen_open: true,
            frame_output_count: 0,
        }
    }
}

// Display byte buffer 16-bit words. Currently used to display PC memory and STACK 
fn hexdump(buf: &[u8], start_addr: u16, as_words: bool) -> String {
    let mut output = String::new();
    let num_word16 = 16;
    let words_per_line  = 4;
    let mut i = 0;
    while i < buf.len()  && i < num_word16*2 {

        if i % (words_per_line*2) == 0 {
            output = output + &format!("{:04X}:", start_addr + (i as u16));
        }
        if i % 2 == 0 {
            output = output + " ";
        }
        output = output + &format!("{:02X}", buf[i]);
        if ((i+1) % (words_per_line*2)) == 0 {
            output = output + "\n";
        }
        i += 1;
    }
    output
}


// copies from egui/examples/custom_font, we want to use Monospace font
fn setup_custom_fonts(ctx: &egui::Context) {
    // Start with the default fonts (we will be adding to them rather than replacing them).
    let mut fonts = egui::FontDefinitions::default();

    // Install my own font (maybe supporting non-latin characters).
    // .ttf and .otf files supported.
    // TODO: create some kind of $RESOURCE_DIR env variable to store fonts
    fonts.font_data.insert(
        "my_font".to_owned(),
        egui::FontData::from_static(include_bytes!(
            "../../fonts/Hack-Regular.ttf"
        )),
    );

    // Put my font first (highest priority) for proportional text:
    fonts
        .families
        .entry(egui::FontFamily::Proportional)
        .or_default()
        .insert(0, "my_font".to_owned());

    // Put my font as last fallback for monospace:
    fonts
        .families
        .entry(egui::FontFamily::Monospace)
        .or_default()
        .push("my_font".to_owned());

    // Tell egui to use these fonts:
    ctx.set_fonts(fonts);
}

fn show_instr(app: &GbcApp, pc: u16) -> String {
    let opcode = app.cpu.load8(pc);
    let (_, disasm) = app.cpu.disasm(opcode);
    format!("{}", disasm)
}

// TODO:
// This is  actually running really slowly, I think its due to us running our emulator inside of
// egui update().
// We should actually have our own main loop, inside our loop we should run
// our emulator. Then we should manually call egui to execute the gui portion.
// I believe egui update() is throttled by the host computer refresh rate.
// Which kind of explains we are only being called at 60 fps.
//
impl eframe::App for GbcApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Gameboy Emulator");

            // Display CPU registers
            ui.label(format!("{:?}", self.cpu));

            // Display IF, IME
            ui.label(format!("IF: {:02X} IE: {:02X} IME:{}", 
                             self.cpu.bus.read8(0xFF0F),
                             self.cpu.bus.read8(0xFFfF),
                             self.cpu.bus.interrupts.prev_ime));
            // Display 4 words of memory at PC address
            let pcbuf = &self.bus.mem[self.cpu.pc() as usize .. self.cpu.pc() as usize + 8];
            let pcbufoutput = hexdump(&pcbuf, self.cpu.pc(), false);
            ui.label("pc memory:");
            ui.label(pcbufoutput);

            // Display stack memory
            ui.label("stack memory:");
            let stack_end = if self.cpu.regs.sp as usize + 16*2+1 > 0xFFFF { 0xFFFF} else { self.cpu.regs.sp as usize + 16*2+1 };
            let stackbuf = &self.bus.mem[self.cpu.regs.sp as usize .. stack_end];
            let stackoutput = hexdump(&stackbuf, self.cpu.regs.sp, true);
            ui.label(&stackoutput); 

            // Display Timer Counter
            ui.label(format!("TIMA: {:4X}", self.cpu.bus.read8(0xFF05)));

            // Display current instruction disassembled. 
            ui.label(show_instr(self, self.cpu.pc()));
         
            // If user entered a hex addresss, then display hex dump
            ui.text_edit_singleline(&mut self.addr_to_dump);
            if let Ok(addr) = u16::from_str_radix(&self.addr_to_dump, 16) {
                let membufoutput = self.cpu.bus.hexdump(addr, 48);
                ui.label("memory:");
                ui.label(membufoutput);
            }



            // If not in continous run mode, display step button
            ui.add_enabled_ui(!self.run_continuous, |ui| {
                // Execute one instruction on step clicked
                if ui.button("Step").clicked() {
                    if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                        self.cpu.step();
                    }
                }
            });
           
            // Prime line output tracking
            let mut line_output_count = self.bus.ppu.line_output_count;
            // Enable/Disable continous running
            ui.checkbox(&mut self.run_continuous, "Run");
            if  self.run_continuous {
                let prev_cycles = self.cycles;
                let mut cycles_accum: u64= 0;

                if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                    // get time before 
                    // there are 17476 cpu ticks  in a frame of 60 FPS
                    while self.frame_output_count == self.bus.ppu.frame_output_count {
                        // DEBUG: Set "breakpoint" on EI instruction.
                        //let opcode = self.cpu.load8(self.cpu.pc());
                        //if opcode == 0xFB {
                        //    println!("pausing at EI");
                        //    self.run_continuous = false;
                        //    break;
                        //}
                        // compute cycle delta
                        let prev_pc = self.cpu.pc();
                        let mut cpu_cycle = self.cpu.step() as u64;
                        if cpu_cycle == 0 {
                            self.run_continuous = false;
                            cpu_cycle = 1; // force it
                            println!("cpu.step returned nothing! prev instr: {}", show_instr(self, prev_pc));
                        }
                        self.cycles  = self.cycles.wrapping_add(cpu_cycle);
                        let mut delta: u64 = if prev_cycles < self.cycles {
                            self.cycles - prev_cycles
                        } else {
                            self.cycles + (std::u64::MAX - prev_cycles)
                        };
                        cycles_accum = cycles_accum.wrapping_add(delta as u64); 





                    }
                    self.frame_output_count = self.bus.ppu.frame_output_count;
                   
                    // sleep for 1/60 of a sec
                    // TODO: magic number
                    let time_per_frame_in_micro =  1.0/(60 as f32) * 1_000_000.0;
                    let frame_time = Duration::from_micros(time_per_frame_in_micro as u64);
                    //std::thread::sleep(frame_time);
                }
            }
            // TODO: This is kind of kludgy, we should probably find some 
            // other way to sync outputting once a whole frame is drawn?
            // Draw GB screen
            let mut is_gb_screen_open = true;
            egui::Window::new("GB Screen").id(egui::Id::new("gbscreen"))
                .default_width(320.0)
                .default_height(288.0)
                .default_pos((500.0,500.0))
                .open(&mut is_gb_screen_open)
                .show(ctx, |ui| {
                    self.screen_ui(ui);
                });

            let mut is_gb_tile_screen_open = true;
            egui::Window::new("GB Tile Data").id(egui::Id::new("gbtiledatascreen"))
                .default_width(256.0)
                .default_height(384.0)
                .default_pos((0.0,500.0))
                .open(&mut is_gb_tile_screen_open)
                .show(ctx, |ui| {
                    self.tile_data_screen_ui(ui);
                });

            ctx.request_repaint();
        });
    }

}

fn main() {
    let mut options = eframe::NativeOptions::default();
    options.initial_window_size = Some(egui::Vec2{x:1200.0, y:1000.0}); 

    eframe::run_native(
        "Gameboy Emulator",
        options,
        Box::new(|cc| Box::new(GbcApp::new(cc))),
        );
}

