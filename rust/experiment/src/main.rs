use std::fmt::Display;

fn display2<T: Display>(x: T) {
    println!("{}", x);
}

fn display(x: Display) {
    println!("{}", x);
}

fn main() {
    let s: &str = "hi there";
    display(34);
}
