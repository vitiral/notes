
use std;
use std::io::prelude::*;
use std::fs::File;

type Result<T> = std::result::Result<T, String>;

fn setup() -> std::io::Result<()>{
    let mut a = try!(File::create("c"));
    try!(a.write_all(b"banana"));
    let mut b = try!(File::create("d"));
    b.write_all(b"pear")
}

fn get_data(path: &str) -> Result<String> {
    let mut file = try!(File::open(path)
                   .map_err(|e| e.to_string()));
    let mut contents = String::new();
    try!(file.read_to_string(&mut contents)
         .map_err(|err| err.to_string()));
    Ok(contents)
}

fn concat(a: &str, b: &str) -> Result<String> {
    // Return the concated strings, else return whichever has the first err
    let data_a = try!(get_data(a));
    let data_b = try!(get_data(b));
    Ok(data_a + &data_b)
}

pub fn function() {
    println!("# Learning try");
    setup().unwrap();
    match concat("c", "d") {
        Ok(n) => println!("Got: {}", n),
        Err(e) => println!("Err: {}", e),
    }
}
