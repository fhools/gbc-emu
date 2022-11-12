use gbc_emu::cpu;
use gbc_emu::bus;
use gbc_emu::rom::read_rom;
use gbc_emu::util::Shared;
use eframe::egui;

struct GbcApp {
    cpu: cpu::LR35902Cpu,
    bus: Shared<bus::Bus>,
    run_continuous: bool,
}

impl GbcApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        setup_custom_fonts(&cc.egui_ctx);
        let rom = read_rom("cpu_instrs.gb").unwrap();
        println!("rom size: {}", rom.len());
        let gbc_bus = Shared::new(bus::Bus::new(&rom));
        let gbc_cpu = cpu::LR35902Cpu::new(0x100, gbc_bus.clone());
        GbcApp {
            cpu: gbc_cpu,
            bus: gbc_bus,
            run_continuous: false,
        }
    }
}

// Display byte buffer 16-bit words. Currently used to display PC memory and STACK 
fn hexdump(buf: &[u8], start_addr: u16, as_words: bool) -> String {
    let mut output = String::new();
    let num_word16 = 16;
    let words_per_line  = 4;
    for i in 0..num_word16{
        if i*2 + 1 > buf.len() {
            break;
        }
        if i % words_per_line == 0 {
            output = output + &format!("{:04X}:", start_addr + (i as u16)*2);
        }
        let word;
        if as_words {
            word = ((buf[i+1] as u16) << 8) | (buf[i] as u16); 
        } else {
            word = ((buf[i] as u16) << 8) | (buf[i+1] as u16); 
        }
        output = output + &format!(" {:04X}", word);
        if ((i+1) % words_per_line) == 0 {
            output = output + "\n";
        }
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

impl eframe::App for GbcApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Gameboy Emulator");

            // Display CPU registers
            ui.label(format!("{:?}", self.cpu));

            // Display 4 words of memory at PC address
            let pcbuf = &self.bus.mem[self.cpu.pc() as usize .. self.cpu.pc() as usize + 8];
            let pcbufoutput = hexdump(&pcbuf, self.cpu.pc(), false);
            ui.label("pc memory:");
            ui.label(pcbufoutput);

            // Display stack memory
            ui.label("stack memory:");
            let stackbuf = &self.bus.mem[self.cpu.regs.sp as usize .. self.cpu.regs.sp as usize + 16*2+1];
            let stackoutput = hexdump(&stackbuf, self.cpu.regs.sp, true);
            ui.label(&stackoutput); 

            // Display current instruction disassembled. 
            let opcode = self.cpu.load8(self.cpu.pc());
            let (_, disasm) = self.cpu.disasm(opcode);
            ui.label(format!("{}", disasm));
           
            // If not in continous run mode, display step button
            ui.add_enabled_ui(!self.run_continuous, |ui| {

                // Execute one instruction on step clicked
                if ui.button("Step").clicked() {
                    if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                        self.cpu.exec_one_instruction() as usize;
                    }
                }
            });

            // Enable/Disable continous running
            ui.checkbox(&mut self.run_continuous, "Run");
            if  self.run_continuous {
                if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                    self.cpu.exec_one_instruction();
                    // Must call request_repaint() otherwise update() won't be triggered unless GUI event
                    // happens
                    ctx.request_repaint();
                }
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

