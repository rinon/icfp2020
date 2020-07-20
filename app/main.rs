use std::env;
use std::io::Error;

use log::debug;

use galaxiator::{Environment, GameResponse};

fn main() -> Result<(), Error> {
    env_logger::init();
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2];

    debug!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    let mut env = Environment::from_galaxy()
        .expect("Could not load galaxy");
    env.make_submission(server_url, player_key.parse().expect("Could not parse player key"));

    let response = GameResponse::parse(&env.join_game());
    dbg!(response);

    let response = GameResponse::parse(&env.start_game(32, 1, 16, 1));
    dbg!(response);

    Ok(())
}
