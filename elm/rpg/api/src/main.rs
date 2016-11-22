extern crate rustc_serialize;
#[macro_use] extern crate nickel;

use nickel::{
    Request, Response, MiddlewareResult,
    Nickel, HttpRouter, MediaType};
use nickel::status::StatusCode;
use rustc_serialize::json;

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

fn player_id_handler<'a> (req: &mut Request, mut res: Response<'a>) -> MiddlewareResult<'a> {
    let players = vec![
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
    ];

    let id: u64 = match req.param("id").expect("invalid url got through").parse() {
        Ok(id) => id,
        Err(err) => {
            println!("- got invalid id");
            return res.send(format!("invalid id: {}", err));
        },
    };
    let player = match players.iter().filter(|p| p.id == id).next() {
        Some(p) => p,
        None => {
            println!("- id not found: {}", id);
            res.set(StatusCode::NotFound);
            return res.send(format!("Player {} not found", id));
        },
    };
    let data = json::as_pretty_json(&player);
    let str_data = format!("{}", data);
    println!("* GET /players/{} -> {}", id, str_data);
    res.send(str_data)
}

fn main() {
    let mut server = Nickel::new();

    // // hello world
    //server.utilize(router! {
    //    get "**" => |_req, _res| {
    //        "Hello World!"
    //    }
    //});

    //// simple routing
	//server.get("/bar", middleware!("This is the /bar handler"));
    //server.get("/user/:userid", middleware! { |request|
    //    format!("This is user: {:?}", request.param("userid"))
    //});
    //server.get("/a/*/d", middleware!("matches /a/b/d but not /a/b/c/d"));
    //server.get("/a/**/d", middleware!("This matches /a/b/d and also /a/b/c/d"));
    
    let players = vec![
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
    ];

    server.get("/players", middleware! { |_, mut res|
        config_res(&mut res);
        let data = json::as_pretty_json(&players);
        println!("* GET /players -> {}", data);
        format!("{}", data)
    });

    server.get("/user/:userid", middleware! { |request|
        format!("This is user: {:?}", request.param("userid"))
    });

    server.get("/players/:id", player_id_handler);

    server.listen("127.0.0.1:4000").expect("canot connect to port");
}
