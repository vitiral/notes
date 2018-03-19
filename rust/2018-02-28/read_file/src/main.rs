extern crate path_abs;

use std::path::Path;
use std::fs;
use std::io::prelude::*;
use std::io;

fn run() -> io::Result<()> {
    let path = Path::new("src/main.rs");
    let meta = path.metadata()?;
    if meta.is_file() {
        let mut file = fs::File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        println!("Contents:\n{}", contents);
    } else {
        panic!("not a file");
    };
    Ok(())
}

fn run2() -> io::Result<()> {
    use path_abs::PathFile;
    let file = PathFile::new("src/main.rs")?;
    println!("Contents:\n{}", file.read_string()?);
    Ok(())
}

fn main() {
    run().unwrap();
    run2().unwrap();

}
