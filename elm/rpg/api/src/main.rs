use std::ops::Deref;
use std::sync::Mutex;

extern crate rustc_serialize;
#[macro_use] extern crate nickel;
#[macro_use] extern crate lazy_static;

use nickel::{
    Request, Response, MiddlewareResult,
    Nickel, HttpRouter, MediaType};
use nickel::status::StatusCode;
use rustc_serialize::json;

lazy_static! {
    #[derive(RustcDecodable, RustcEncodable)]
    static ref PLAYERS: Mutex<Vec<Player>> = Mutex::new(vec![
        Player {
            id: 1,
            name: "Sally".to_string(),
            level: 2,
        },
        Player {
            id: 2,
            name: "Lance".to_string(),
            level: 1,
        },
    ]);
}

#[derive(RustcDecodable, RustcEncodable)]
struct Player {
    pub id: u64,
    pub name: String,
    pub level: i64,
}

fn config_res(res: &mut Response) {
    res.set(MediaType::Json);
    res.set(StatusCode::Ok);
    {
        let head = res.headers_mut();
        head.set_raw("Access-Control-Allow-Origin", vec![Vec::from("*".as_bytes())]);
    }
}

fn parse_id(req: &Request) -> Result<u64, String> {
    match req.param("id").expect("invalid url got through").parse() {
        Ok(id) => Ok(id),
        Err(err) => {
            println!("- got invalid id");
            Err(format!("invalid id: {}", err))
        },
    }
}

fn get_player<'a>(players: &'a mut Vec<Player>, id: u64) -> Result<&'a mut Player, String> {
    match players.iter_mut().filter(|p| p.id == id).next() {
        Some(p) => Ok(p),
        None => {
            println!("- id not found: {}", id);
            Err(format!("Player {} not found", id))
        },
    }
}

fn get_player_id<'a> (req: &mut Request, mut res: Response<'a>) 
        -> MiddlewareResult<'a> 
{
    let id = match parse_id(req) {
        Ok(id) => id,
        Err(e) => return res.send(e),
    };
    let mut locked = PLAYERS.lock().unwrap();
    let player = match get_player(locked.as_mut(), id) {
        Ok(p) => p,
        Err(e) => {
            res.set(StatusCode::NotFound);
            return res.send(e);
        },
    };
    let data = json::as_pretty_json(player);
    let str_data = format!("{}", data);
    println!("* GET /players/{} -> {}", id, str_data);
    res.send(str_data)
}

fn put_player_id<'a> (req: &mut Request, mut res: Response<'a>) 
        -> MiddlewareResult<'a> 
{
    let id: u64 = match req.param("id").expect("invalid url got through").parse() {
        Ok(id) => id,
        Err(err) => {
            println!("- got invalid id");
            return res.send(format!("invalid id: {}", err));
        },
    };
    let locked = PLAYERS.lock().unwrap();
    let player = match locked.iter().filter(|p| p.id == id).next() {
        Some(p) => p,
        None => {
            println!("- id not found: {}", id);
            res.set(StatusCode::NotFound);
            return res.send(format!("Player {} not found", id));
        },
    };
    let data = json::as_pretty_json(player);
    let str_data = format!("{}", data);
    println!("* GET /players/{} -> {}", id, str_data);
    res.send(str_data)
}

fn main() {
    let mut server = Nickel::new();

    server.get("/players", middleware! { |_, mut res|
        config_res(&mut res);
        let locked = PLAYERS.lock().unwrap();
        let data = json::as_pretty_json(locked.deref());
        println!("* GET /players -> {}", data);
        format!("{}", data)
    });

    server.get("/user/:userid", middleware! { |request|
        format!("This is user: {:?}", request.param("userid"))
    });

    server.get("/players/:id", get_player_id);

    //server.put("/players/:id", put_player_id);

    server.listen("127.0.0.1:4000").expect("canot connect to port");
}
