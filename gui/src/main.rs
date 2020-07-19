use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::mouse::MouseButton;
use sdl2::pixels::Color;
use sdl2::{rect::Point, render::BlendMode};

use std::collections::HashSet;
use std::time::Duration;

use galaxiator::{Environment, State};

const DIMENSIONS: (i32, i32) = (1600, 1200);
const SCALE: i32 = 4;

pub fn main() -> Result<(), String> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let mut galaxy = Environment::from_file(concat!(env!("CARGO_MANIFEST_DIR"), "/../galaxy.txt"))
        .expect("Could not open galaxy.txt");
    let mut images = galaxy.interact((0, 0));
    eprintln!("{:?}", images);

    let window = video_subsystem
        .window(
            "rust-sdl2 demo: Video",
            DIMENSIONS.0 as u32,
            DIMENSIONS.1 as u32,
        )
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window.into_canvas().build().map_err(|e| e.to_string())?;

    canvas.set_scale(SCALE as f32, SCALE as f32)?;
    canvas.set_draw_color(Color::RGB(32, 32, 32));
    canvas.set_blend_mode(BlendMode::Blend);
    canvas.clear();
    canvas.present();
    let mut events = sdl_context.event_pump()?;

    let mut prev_buttons = HashSet::new();
    let mut prev_keys = HashSet::new();

    let colors = &[
        Color::RGBA(255, 0, 0, 192),
        Color::RGBA(0, 255, 0, 192),
        Color::RGBA(0, 0, 255, 192),
    ];

    let mut count = 0;
    'running: loop {
        for event in events.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        // Create a set of pressed Keys.
        let keys = events
            .keyboard_state()
            .pressed_scancodes()
            .filter_map(Keycode::from_scancode)
            .collect();

        // Get the difference between the new and old sets.
        let new_keys = &keys - &prev_keys;
        prev_keys = keys;

        if !new_keys.is_empty() {
            if new_keys.contains(&Keycode::Space) {
                galaxy.dump_state();
            }
            if new_keys.contains(&Keycode::Num0) {
                galaxy.set_state(State::Initial);
                images = galaxy.interact((0, 0));
                continue;
            }
            if new_keys.contains(&Keycode::Num1) {
                galaxy.set_state(State::Menu);
                images = galaxy.interact((0, 0));
                continue;
            }
        }

        // get a mouse state
        let state = events.mouse_state();

        // Create a set of pressed Keys.
        let buttons = state.pressed_mouse_buttons().collect();

        // Get the difference between the new and old sets.
        let new_buttons = &buttons - &prev_buttons;

        if new_buttons.contains(&MouseButton::Left) {
            let (x, y) = rev_translate((state.x(), state.y()));
            images = galaxy.interact((x, y));
            // } else {
            //     count += 1;
            //     if count == 30 {
            //         count = 0;
            //         images = galaxy.increment();
            //     }
        }

        prev_buttons = buttons;

        canvas.set_draw_color(Color::RGB(32, 32, 32));
        canvas.clear();
        for (image, color) in images.iter().zip(colors.iter()) {
            canvas.set_draw_color(*color);
            let image: Vec<Point> = image.iter().map(|point| translate(*point).into()).collect();
            canvas
                .draw_points(&image[..])
                .expect("Could not draw image");
        }
        canvas.present();

        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 30));
    }

    Ok(())
}

fn translate((x, y): (i32, i32)) -> (i32, i32) {
    (x + DIMENSIONS.0 / 2 / SCALE, y + DIMENSIONS.1 / 2 / SCALE)
}

fn rev_translate((x, y): (i32, i32)) -> (i32, i32) {
    (
        (x - DIMENSIONS.0 / 2) / SCALE,
        (y - DIMENSIONS.1 / 2) / SCALE,
    )
}
