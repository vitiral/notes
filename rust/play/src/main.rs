use std::char;
use std::io;
use std::fs;
use std::iter::FromIterator;

use std::sync::mpsc::TryRecvError;

fn into_try_recv() {
    let recv_err = TryRecvError::Disconnected;
    let e: TryRecvError = recv_err.into();
    println!("try_recv?:\n- {}\n- {}", e, recv_err);
}

fn read_dne() -> io::Result<()> {
    let path = "foobar";
    fs::File::open(path)
        .map_err(|err| io::Error::new(err.kind(), format!("(path={}) {}", path, err)))?;
    Ok(())
}

fn fmt_fn() {
    println!("This is a function: {}", {
        let x = 10;
        10 + 7
    });
}

fn main() {
    if let Err(e) = read_dne() {
        println!("Got Error: {}", e);
    }

    into_try_recv();

    println!("Hello, world!");
    let dig = 'F'.to_digit(16).unwrap();
    println!("F: {}", dig);
    // println!("x09 == {}", "x09".parse::<u8>().unwrap());
}
