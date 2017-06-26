use std;
use std::io::prelude::*;
use std::fs::File;

type Result<T> = std::result::Result<T, String>;

// Setup to make this work, create two files with the same info,
// we use unwrap because if this fails the whole program
// *should* crash!
fn setup() {
    File::create("a")
        .and_then(|mut file| file.write_all(b"grape"))
        .unwrap();

    File::create("b")
        .and_then(|mut file| file.write_all(b"fruit"))
        .unwrap();
}

/// get the data from a path
fn get_data(path: &str) -> Result<String> {
    let out = File::open(path)
        .map_err(|err| err.to_string())
        .and_then(|mut file| {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|err| err.to_string())
                .map(|_| contents) // ignore the output "read to string" returns and just return contents
        });
    println!("Got {}: {:?}", path, out);
    out
}

fn concat(a: &str, b: &str) -> Result<String> {
    // Return the concated strings, else return whichever has the first err
    get_data(a).and_then(|a| get_data(b).and_then(|b| Ok(a + &b)))
}


pub fn function() {
    println!("# Learning more errors");
    setup();
    match concat("a", "b") {
        Ok(n) => println!("Got: {}", n),
        Err(e) => println!("Err: {}", e),
    }
}
