#![feature(proc_macro, generators)]

extern crate futures_await as futures;
extern crate tokio_core;

use futures::prelude::*;
use futures::future::{err, ok};
use tokio_core::reactor::Core;
use std::error::Error;

fn fut_hello() -> impl Future <Item=u32, Error=()> {
    println!("Hello from the future!");

    ok(42)
}

fn main() {
    let mut reactor = Core::new().unwrap();
    println!("Hello, world!");

    let handle = fut_hello();
    println!("Hello from present!");
    let retval = reactor.run(handle).unwrap();

    println!("ret: {}", retval);

}
