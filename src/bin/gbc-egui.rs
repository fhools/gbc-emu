use gbc_emu::cpu::{self, LR35902Cpu};
use gbc_emu::bus::{self, Bus, Interrupts};
use gbc_emu::ppu::Ppu;
use gbc_emu::rom::read_rom;
use gbc_emu::util::Shared;
use gbc_emu::bootrom::BOOT_ROM;
use eframe::egui;
use std::time::Duration;

struct GbcApp {
    cpu: cpu::LR35902Cpu,
    bus: Shared<bus::Bus>,
    run_continuous: bool,
    addr_to_dump: String,
    cycles: u64,
}

impl GbcApp {
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

        // The timer ISR to run
        let isr_FF50 = [
            0x03, // inc h
            0xD9, // RETI
        ];

        let mut code_buffer = vec![0u8; 0xFFFF];
        code_buffer[0..main_program.len()].copy_from_slice(&main_program[..]);
        code_buffer[0xFF50..0xFF50 + 2].copy_from_slice(&isr_FF50[..]);

        let rom = read_rom("roms/cpu_instrs.gb").unwrap();
        println!("rom size: {}", rom.len());

        let mem = Shared::new(vec![0u8; 0x10000]);
        let interrupts = Shared::new(Interrupts::default());
        let ppu = Shared::new(Ppu::new(interrupts.clone()));
        //let bus = Shared::new(Bus::new(&code_buffer, mem.clone(), interrupts.clone(), ppu.clone()));
        let bus = Shared::new(Bus::new(&rom, &BOOT_ROM, mem.clone(), interrupts.clone(), ppu.clone()));
        // For ISR test program starts at 0x0
        //let cpu = LR35902Cpu::new(0x0, bus.clone());

        let cpu = LR35902Cpu::new(0x0, bus.clone());
    
        GbcApp {
            cpu: cpu,
            bus: bus,
            run_continuous: false,
            addr_to_dump: String::new(),
            cycles: 0
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
                let membufoutput = self.cpu.bus.hexdump(addr, 16);
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

            // Enable/Disable continous running
            ui.checkbox(&mut self.run_continuous, "Run");
            if  self.run_continuous {
                let prev_cycles = self.cycles;
                let mut cycles_accum: u64= 0;

                if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                    // get time before 
                    // there are 17476 cpu ticks  in a frame of 60 FPS
                    while cycles_accum < 17476 {
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
                   
                    // get time after gb


                    // sleep for 1/60 of a sec
                    // TODO: magic number
                    let time_per_frame_in_micro =  1.0/(60 as f32) * 1_000_000.0;
                    let frame_time = Duration::from_micros(time_per_frame_in_micro as u64);
                    //std::thread::sleep(frame_time);
                }
                // Must call request_repaint() otherwise update() won't be triggered unless GUI event
                // happens
                ctx.request_repaint();
            }
        });
    }
}

fn main() {
    let options = eframe::NativeOptions::default();

    eframe::run_native(
        "Gameboy Emulator",
        options,
        Box::new(|cc| Box::new(GbcApp::new(cc))),
        );
}

