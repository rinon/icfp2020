use std::env;
use std::io::Error;

use log::debug;

use galaxiator::{Environment, GameResponse, GameStage};

fn main() -> Result<(), Error> {
    env_logger::init();
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2];

    debug!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    let mut env = Environment::from_galaxy()
        .expect("Could not load galaxy");
    env.make_submission(server_url, player_key.parse().expect("Could not parse player key"));

    let mut response = GameResponse::parse(&env.join_game())
        .expect("Could not parse join response");
    dbg!(response);

    response = GameResponse::parse(&env.start_game(32, 1, 16, 1))
        .expect("Could not parse start response");
    dbg!(&response);

    while response.stage != GameStage::Finished {
        response = GameResponse::parse(&env.send_commands(&[]))
            .expect("Could not parse game response");
    }

    Ok(())
}
