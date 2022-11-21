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
    const WIDTH: f32;
    const HEIGHT: f32;
    fn screen_ui(&mut self, ui: &mut egui::Ui); 
}

impl MyView for MyApp {
    const WIDTH: f32 = 320.0;
    const HEIGHT: f32 = 240.0;
    fn screen_ui(&mut self, ui: &mut egui::Ui) {
        ui.label(format!("window size: width: {}, height: {}", Self::WIDTH, Self::HEIGHT));
        // allocate space in the child window
        let (response, painter) = ui.allocate_painter(Vec2::new(Self::WIDTH, Self::HEIGHT), Sense::hover());
        let Vec2{x: width, y: height} = response.rect.size();
        let colors = [color::Color32::RED, color::Color32::YELLOW, color::Color32::DARK_GREEN, color::Color32::BLACK,
                      color::Color32::GOLD, color::Color32::BLUE];

        // create a transform to go from local space to global window space
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, response.rect.size()),
            response.rect,
            );

        // output all the pixels, i'm using rects because i dont know any better way. seems kludgy
        for i in 0..(width as i32) {
            for j in 0..(height as i32) {
                let rect = Rect::from_min_max([i as f32, j as f32].into(), [(i+1) as f32, (j+1) as f32].into());
                painter.add(egui::Shape::rect_filled(to_screen.transform_rect(rect), 
                                                     0.0, 
                                                     colors[((i as usize + j as usize) % colors.len()) as usize]));
            }
        }
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
