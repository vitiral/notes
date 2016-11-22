extern crate rustc_serialize;
#[macro_use] extern crate nickel;

use nickel::{Nickel, HttpRouter, JsonBody, MediaType};
use nickel::status::StatusCode;
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Player {
    //pub id: u64,
    pub id: String,
    pub name: String,
    pub level: i64,
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
            id: "1".to_string(),
            name: "Sally".to_string(),
            level: 2,
        },
        Player {
            id: "2".to_string(),
            name: "Lance".to_string(),
            level: 1,
        },
    ];

    server.get("/players", middleware! { |_, mut res|
        res.set(MediaType::Json);
        res.set(StatusCode::Ok);
        {
            let head = res.headers_mut();
            head.set_raw("Access-Control-Allow-Origin", vec![Vec::from("*".as_bytes())]);
        }

        let data = json::as_pretty_json(&players);
        println!("{:?}", res.headers());
        format!("{}", data)
    });

    server.listen("127.0.0.1:4000").expect("canot connect to port");
}
