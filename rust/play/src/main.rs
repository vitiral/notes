use std::process::Command;
use std::path::*;

fn join<P: AsRef<Path>>(a: P, b: P) {
    println!("Joining: {} with {}", a.as_ref().display(), b.as_ref().display());
    let result = a.as_ref().join(b);
    println!("Result : {}", result.display());

}

fn main() {
    let hostname = Command::new("hostname")
        .output()
        .expect("could not get hostname")
        .stdout;
    let hostname = ::std::str::from_utf8(&hostname).unwrap();
    println!("Hostname: {}", hostname.trim());

    join("a", "b");
    join("a/", "b");
    join("/a", "b");
    join("a", "/b");
    join("/a", "/b");
}
