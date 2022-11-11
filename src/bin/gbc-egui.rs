use gbc_emu::cpu;
use gbc_emu::bus;
use gbc_emu::rom::read_rom;
use gbc_emu::util::Shared;
use eframe::egui;

struct GbcApp {
    cpu: cpu::LR35902Cpu,
    bus: Shared<bus::Bus>,
    instruction_run_limit: i32,
    run_continuous: bool,
}

impl Default for GbcApp {
    fn default() -> Self {
        let rom = read_rom("cpu_instrs.gb").unwrap();
        println!("rom size: {}", rom.len());
        println!("rom: {:?}", rom);
        let gbc_bus = Shared::new(bus::Bus::new(&rom));
        let mut gbc_cpu = cpu::LR35902Cpu::new(0x100, gbc_bus.clone());
        GbcApp {
            cpu: gbc_cpu,
            bus: gbc_bus,
            instruction_run_limit: 0,
            run_continuous: false,
        }
    }
}


impl eframe::App for GbcApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Gameboy Emulator");
            ui.label(format!("{:?}", self.cpu));
            let opcode = self.cpu.load8(self.cpu.pc());
            let (_, disasm) = self.cpu.disasm(opcode);
            ui.label(format!("{}", disasm));
            if ui.button("Step").clicked() {
                if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                    self.cpu.exec_one_instruction() as usize;
                }
            }
            ui.checkbox(&mut self.run_continuous, "Run");
            if  self.run_continuous {
                if (self.cpu.pc() as i32) < (self.bus.mem.len() as i32) {
                    self.cpu.exec_one_instruction() as usize;
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
        Box::new(|_cc| Box::new(GbcApp::default())),
        );
}

