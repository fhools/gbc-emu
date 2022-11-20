#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release


// This is my egui playground to work out how to convert a gameboy frame buffer output bt the PPU
// into pixels on the screen in egui

use eframe::egui;
use egui::*;
use egui::epaint::{Rect, RectShape};
fn main() {
    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "My egui App",
        options,
        Box::new(|_cc| Box::new(MyApp::default())),
    );
}

struct MyApp {
    name: String,
    age: u32,
    screen_open: bool,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            name: "Arthur".to_owned(),
            age: 42,
            screen_open: true,
        }
    }
}
trait MyView {
    fn screen_ui(&mut self, ui: &mut egui::Ui); 
}

impl MyView for MyApp {
    fn screen_ui(&mut self, ui: &mut egui::Ui) {
        ui.label("My screen label");
        let rect = Rect::from_min_max([0.0,0.0].into(), [1.0, 1.0].into());
        let (response, painter) = ui.allocate_painter(Vec2::new(ui.available_width(), ui.available_height()), Sense::hover());
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, response.rect.size()),
            response.rect,
        );
        painter.add(egui::Shape::rect_filled(to_screen.transform_rect(rect), 0.0, color::Color32::RED));
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("My egui Application");
            ui.horizontal(|ui| {
                ui.label("Your name: ");
                ui.text_edit_singleline(&mut self.name);
            });
            ui.add(egui::Slider::new(&mut self.age, 0..=120).text("age"));
            if ui.button("Click each year").clicked() {
                self.age += 1;
            }
            ui.label(format!("Hello '{}', age {}", self.name, self.age));
            let mut is_open = true;
            egui::Window::new("Screen")
                .default_width(320.0)
                .default_height(240.0)
                .open(&mut is_open)
                .show(ctx, |ui| {
                    self.screen_ui(ui);
                });
        });
    }
}
