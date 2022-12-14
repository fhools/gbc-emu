use gbc_emu::cpu::{self, LR35902Cpu};
use gbc_emu::bus::{self, Bus, Interrupts};
use gbc_emu::ppu::{self, Ppu};
use gbc_emu::rom::read_rom;
use gbc_emu::util::Shared;
use gbc_emu::bootrom::BOOT_ROM;
use eframe::egui;
use egui::*;
use std::time::Duration;

struct GbcApp {
    cpu: cpu::LR35902Cpu,
    bus: Shared<bus::Bus>,
    run_continuous: bool,
    addr_to_dump: String,
    cycles: u64,
    frame_output_count: u32,
}

impl GbcApp {
    const COLORS: [color::Color32; 4] = [ color::Color32::from_rgb(255, 255, 255),
                                          color::Color32::from_rgb(170, 170, 170), 
                                          color::Color32::from_rgb(85, 85, 85),
                                          color::Color32::from_rgb(0, 0, 0)];

    fn screen_ui(&mut self, ui: &mut egui::Ui) {
        // allocate space in the child window
        let (response, painter) = ui.allocate_painter(Vec2::new(640.0, 576.0), Sense::hover());

        let gb_screen_size = Vec2::new(160.0, 144.0);
        // create a transform to go from local space to global window space
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, gb_screen_size),
            response.rect,
            );

        // output all the pixels, i'm using rects because i dont know any better way. seems kludgy
        for j in 0..(ppu::SCREEN_HEIGHT_PX as i32) {
            for i in 0..(ppu::SCREEN_WIDTH_PX as i32) {
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
        let (response, painter) = ui.allocate_painter(Vec2::new(512.0, 768.0), Sense::hover());

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

    fn get_input(&mut self, ctx: &egui::Context) -> bus::InputButtons {
        let mut button_inputs: bus::InputButtons = Default::default();
        button_inputs.key_up = ctx.input().key_down(egui::Key::ArrowUp);
        button_inputs.key_down = ctx.input().key_down(egui::Key::ArrowDown);
        button_inputs.key_left = ctx.input().key_down(egui::Key::ArrowLeft);
        button_inputs.key_right = ctx.input().key_down(egui::Key::ArrowRight);
        button_inputs.key_a = ctx.input().key_down(egui::Key::A);
        button_inputs.key_b = ctx.input().key_down(egui::Key::B);
        button_inputs.key_select = ctx.input().key_down(egui::Key::Space);
        button_inputs.key_start = ctx.input().key_down(egui::Key::Enter);
        button_inputs
    }

    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        setup_custom_fonts(&cc.egui_ctx);

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
            frame_output_count: 0,
        }
    }
}

// Display byte buffer 16-bit words. Currently used to display PC memory and STACK 
fn hexdump(buf: &[u8], start_addr: u16) -> String {
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

    // We want fixed width font to display hex dumps 
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
            let pcbufoutput = hexdump(&pcbuf, self.cpu.pc());
            ui.label("pc memory:");
            ui.label(pcbufoutput);

            // Display stack memory
            ui.label("stack memory:");
            let stack_end = if self.cpu.regs.sp as usize + 16*2+1 > 0xFFFF { 0xFFFF} else { self.cpu.regs.sp as usize + 16*2+1 };
            let stackbuf = &self.bus.mem[self.cpu.regs.sp as usize .. stack_end];
            let stackoutput = hexdump(&stackbuf, self.cpu.regs.sp);
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

            // Grab the input state and send button state to the Bus.
            // This is where the emulation maps host keyboard to gameboy buttons gets done
            let button_input = self.get_input(ctx);
            self.cpu.bus.set_input(&button_input);

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
                        let delta: u64 = if prev_cycles < self.cycles {
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
                    let _frame_time = Duration::from_micros(time_per_frame_in_micro as u64);
                    //std::thread::sleep(frame_time);
                }
            }

            // Draw GB screen
            let mut is_gb_screen_open = true;
            egui::Window::new("GB Screen").id(egui::Id::new("gbscreen"))
                .default_width(640.0)
                .default_height(576.0)
                .default_pos((0.0,600.0))
                .open(&mut is_gb_screen_open)
                .show(ctx, |ui| {
                    self.screen_ui(ui);
                });

            let mut is_gb_tile_screen_open = true;
            egui::Window::new("GB Tile Data").id(egui::Id::new("gbtiledatascreen"))
                .default_width(512.0)
                .default_height(768.0)
                .default_pos((700.0,400.0))
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
    options.initial_window_size = Some(egui::Vec2{x:1400.0, y:1200.0}); 

    eframe::run_native(
        "Gameboy Emulator",
        options,
        Box::new(|cc| Box::new(GbcApp::new(cc))),
        );
}

