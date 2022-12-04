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
    y: u32,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            y: 0
        }
    }
}
trait MyView {
    const WIDTH: f32;
    const HEIGHT: f32;
    const COLORS: [color::Color32; 4];
    fn screen_ui(&mut self, ui: &mut egui::Ui); 
}

impl MyView for MyApp {
    const WIDTH: f32 = 160.0;
    const HEIGHT: f32 = 144.0;
    const COLORS: [color::Color32; 4] = [ color::Color32::from_rgb(255, 255, 255),
                                          color::Color32::from_rgb(170, 170, 170), 
                                          color::Color32::from_rgb(85, 85, 85),
                                          color::Color32::from_rgb(0, 0, 0)];

    fn screen_ui(&mut self, ui: &mut egui::Ui) {
        ui.label(format!("window size: width: {}, height: {}", Self::WIDTH, Self::HEIGHT));
        // allocate space in the child window
        let (response, painter) = ui.allocate_painter(Vec2::new(Self::WIDTH, Self::HEIGHT), Sense::hover());
        let Vec2{x: width, y: height} = response.rect.size();

        // create a transform to go from local space to global window space
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, response.rect.size()),
            response.rect,
            );

        // output all the pixels, i'm using rects because i dont know any better way. seems kludgy
        for i in 0..(width as i32) {
            for j in 0..(height as i32) {
                // output a rect that represents a pixel, coordinates are local to the window
                let rect = Rect::from_min_max([i as f32, j as f32].into(), [(i+1) as f32, (j+1) as f32].into());

                // draw the pixel, output to the window, painter wants global coordinates so use
                // to_screen.tranform_rect
                painter.add(egui::Shape::rect_filled(to_screen.transform_rect(rect), 
                                                     0.0, 
                                                     Self::COLORS[((i as usize + j as usize) % Self::COLORS.len()) as usize]));
            }
        }
    }
}



impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("My egui Application");
            let mut is_open = true;
            egui::Window::new("Screen")
                .default_width(160.0)
                .default_height(144.0)
                .open(&mut is_open)
                .show(ctx, |ui| {
                    self.screen_ui(ui);
                });
        });
    }
}
