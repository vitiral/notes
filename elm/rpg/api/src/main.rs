use std::ops::Deref;
use std::sync::Mutex;

extern crate rustc_serialize;
#[macro_use] extern crate nickel;
#[macro_use] extern crate lazy_static;

use nickel::{
    Request, Response, MiddlewareResult,
    Nickel, HttpRouter, MediaType,
    // Traits
    JsonBody};
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

#[derive(RustcDecodable, RustcEncodable, Eq, PartialEq)]
struct Player {
    pub id: u64,
    pub name: String,
    pub level: i64,
}

fn setup_headers(res: &mut Response) {
    let head = res.headers_mut();
    let bv = |s: &str| Vec::from(s.as_bytes());
    head.set_raw("Access-Control-Allow-Origin", vec![bv("*")]);
    head.set_raw("Access-Control-Allow-Methods",
                 vec![bv("GET, POST, OPTIONS, PUT, PATCH, DELETE")]);
    head.set_raw("Access-Control-Allow-Headers", 
                 vec![bv("X-Requested-With,content-type")]);
}

fn config_json_res(res: &mut Response) {
    res.set(MediaType::Json);
    res.set(StatusCode::Ok);
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
    setup_headers(&mut res);
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
    config_json_res(&mut res);
    res.send(str_data)
}

fn put_player_id<'a> (req: &mut Request, mut res: Response<'a>) 
        -> MiddlewareResult<'a> 
{
    println!("* PUT players/:id start");
    setup_headers(&mut res);
    let id = match parse_id(req) {
        Ok(id) => id,
        Err(e) => {
            res.set(StatusCode::NotFound);
            return res.send(e);
        },
    };
    let mut locked = PLAYERS.lock().unwrap();
    let player = match get_player(locked.as_mut(), id) {
        Ok(p) => p,
        Err(e) => {
            res.set(StatusCode::NotFound);
            return res.send(e);
        },
    };
    let new = match req.json_as::<Player>() {
        Ok(p) => p,
        Err(e) => {
            res.set(StatusCode::BadRequest);
            return res.send(format!("{}", e));
        },
    };
    if new.id != id {
        res.set(StatusCode::BadRequest);
        return res.send("cannot change player's id");
    }
    if new == *player {
        res.set(StatusCode::NotModified);
        return res.send("not modified");
    }
    *player = new;
    let data = json::as_pretty_json(player);
    let str_data = format!("{}", data);
    println!("* PUT /players/{} success", id);
    config_json_res(&mut res);
    res.send(str_data)
}

fn main() {
    let mut server = Nickel::new();

    server.get("/players", middleware! { |_, mut res|
        setup_headers(&mut res);
        let locked = PLAYERS.lock().unwrap();
        let data = json::as_pretty_json(locked.deref());
        println!("* GET /players");
        config_json_res(&mut res);
        format!("{}", data)
    });

    server.get("/user/:userid", middleware! { |request|
        format!("This is user: {:?}", request.param("userid"))
    });
    server.get("/players/:id", get_player_id);
    server.options("/players/:id", middleware! { |_, mut res|
        setup_headers(&mut res);
        res.set(StatusCode::Ok);
        "allow"
    });
    server.put("/players/:id", put_player_id);

    server.listen("127.0.0.1:4000").expect("canot connect to port");
}
