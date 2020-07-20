use clap::{App, Arg};
use image::{GrayImage, ImageFormat, Luma};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::mouse::MouseButton;
use sdl2::pixels::Color;
use sdl2::{rect::Point, render::BlendMode};

use std::collections::HashSet;
use std::process::Command;
use std::sync::atomic::{AtomicI32, Ordering};
use std::time::Duration;

use galaxiator::{Environment, State};

const DIMENSIONS: (i32, i32) = (3440, 1440);
static SCALE: AtomicI32 = AtomicI32::new(4);

const BACKGROUND_COLOR: Color = Color::RGBA(32, 32, 32, 255);

pub fn main() -> Result<(), String> {
    env_logger::init();

    let args = App::new("Galaxiator GUI")
        .author("Stephen Crane")
        .arg(Arg::with_name("tutorial").takes_value(true))
        .get_matches();

    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let mut galaxy = Environment::from_galaxy().expect("Could not open galaxy.txt");
    let mut images = galaxy.click((0, 0));
    if let Some(tutorial) = args.value_of("tutorial") {
        images = galaxy.start_tutorial(
            tutorial
                .parse()
                .expect("Expected an integer tutorial number"),
        );
    }

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

    canvas.set_scale(
        SCALE.load(Ordering::Relaxed) as f32,
        SCALE.load(Ordering::Relaxed) as f32,
    )?;
    canvas.set_draw_color(BACKGROUND_COLOR);
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
        let keys: HashSet<Keycode> = events
            .keyboard_state()
            .pressed_scancodes()
            .filter_map(Keycode::from_scancode)
            .collect();

        // Holding spacebar just clicks at (0,0)
        if keys.contains(&Keycode::Space) {
            images = galaxy.click((0, 0));
        }

        // Get the difference between the new and old sets.
        let new_keys = &keys - &prev_keys;

        handle_input(&mut galaxy, &mut images, &keys, new_keys);

        prev_keys = keys;

        // get a mouse state
        let state = events.mouse_state();

        // Create a set of pressed Keys.
        let buttons = state.pressed_mouse_buttons().collect();

        // Get the difference between the new and old sets.
        let new_buttons = &buttons - &prev_buttons;

        // if new_buttons.contains(&MouseButton::Right) {
        //     let (x, y) = (state.x(), state.y());
        //     let raw_pixels = canvas.read_pixels(Rect::from_center((x,y), 8, 8), PixelFormatEnum::RGBA32)
        //         .expect("Couldn't read from canvas");
        //     let mut pixels = vec![];
        //     let mut i = 0;
        //     for _x in 0..8 {
        //         pixels.push(vec![false; 8]);
        //     }
        //     for y in 0..8 {
        //         for x in 0..8 {
        //             let is_color = raw_pixels[i] != BACKGROUND_COLOR.r ||
        //                 raw_pixels[i+1] != BACKGROUND_COLOR.g ||
        //                 raw_pixels[i+2] != BACKGROUND_COLOR.b;
        //             pixels[x][y] = is_color;
        //             i += 4;
        //         }
        //     }
        //     dbg!(pixels);
        // }

        if new_buttons.contains(&MouseButton::Left) {
            let (x, y) = rev_translate((state.x(), state.y()));
            images = galaxy.click((x, y));
        }

        prev_buttons = buttons;

        canvas.set_scale(
            SCALE.load(Ordering::Relaxed) as f32,
            SCALE.load(Ordering::Relaxed) as f32,
        )?;

        canvas.set_draw_color(BACKGROUND_COLOR);
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

fn handle_input(
    galaxy: &mut Environment,
    images: &mut Vec<Vec<(i32, i32)>>,
    keys: &HashSet<Keycode>,
    new_keys: HashSet<Keycode>,
) {
    if new_keys.is_empty() {
        return;
    }
    if new_keys.contains(&Keycode::S) {
        save_images(images);
    }
    if new_keys.contains(&Keycode::D) {
        galaxy.toggle_debug();
    }
    if new_keys.contains(&Keycode::Return) {
        galaxy.print_state();
    }
    if new_keys.contains(&Keycode::Equals) {
        SCALE.fetch_add(1, Ordering::SeqCst);
    }
    if new_keys.contains(&Keycode::Minus) {
        SCALE.fetch_sub(1, Ordering::SeqCst);
    }
    if keys.contains(&Keycode::T) {
        if new_keys.contains(&Keycode::Num0) {
            *images = galaxy.start_tutorial(0);
        }
        if new_keys.contains(&Keycode::Num1) {
            *images = galaxy.start_tutorial(1);
        }
        if new_keys.contains(&Keycode::Num2) {
            *images = galaxy.start_tutorial(2);
        }
        if new_keys.contains(&Keycode::Num3) {
            *images = galaxy.start_tutorial(3);
        }
        if new_keys.contains(&Keycode::Num4) {
            *images = galaxy.start_tutorial(4);
        }
        if new_keys.contains(&Keycode::Num5) {
            *images = galaxy.start_tutorial(5);
        }
        if new_keys.contains(&Keycode::Num6) {
            *images = galaxy.start_tutorial(6);
        }
        if new_keys.contains(&Keycode::Num7) {
            *images = galaxy.start_tutorial(7);
        }
        if new_keys.contains(&Keycode::Num8) {
            *images = galaxy.start_tutorial(8);
        }
        if new_keys.contains(&Keycode::Num9) {
            *images = galaxy.start_tutorial(9);
        }
        return;
    }
    if new_keys.contains(&Keycode::Num0) {
        galaxy.set_state(State::Initial);
        *images = galaxy.click((0, 0));
    }
    if new_keys.contains(&Keycode::Num1) {
        galaxy.set_state(State::Menu);
        *images = galaxy.click((0, 0));
    }
    if new_keys.contains(&Keycode::Num2) {
        galaxy.set_state(State::Multiplayer);
        *images = galaxy.click((0, 0));
    }
    if new_keys.contains(&Keycode::Num3) {
        galaxy.set_state(State::Join(1113939892088752268));
        *images = galaxy.click((0, 0));
    }
    // if new_keys.contains(&Keycode::J) {
    //     galaxy.join_new_game();
    //     *images = galaxy.click((0, 0));
    // }
}

fn translate((x, y): (i32, i32)) -> (i32, i32) {
    (
        x + DIMENSIONS.0 / 2 / SCALE.load(Ordering::Relaxed),
        y + DIMENSIONS.1 / 2 / SCALE.load(Ordering::Relaxed),
    )
}

fn rev_translate((x, y): (i32, i32)) -> (i32, i32) {
    (
        (x - DIMENSIONS.0 / 2) / SCALE.load(Ordering::Relaxed),
        (y - DIMENSIONS.1 / 2) / SCALE.load(Ordering::Relaxed),
    )
}

fn save_images(images: &Vec<Vec<(i32, i32)>>) {
    let width = 400;
    let height = 300;
    let mut img = GrayImage::new(width, height);
    for image in images {
        for pixel in image {
            img.put_pixel(
                (pixel.0 + width as i32 / 2) as u32,
                (pixel.1 + height as i32 / 2) as u32,
                Luma([255]),
            );
        }
    }
    img.save_with_format("capture.bmp", ImageFormat::Bmp)
        .unwrap();
    Command::new(concat!(env!("CARGO_MANIFEST_DIR"), "/annotate_image.py"))
        .args(&["capture.bmp", "capture.svg"])
        .spawn()
        .expect("Could not run annotater")
        .wait()
        .unwrap();
    println!("Image saved");
}
