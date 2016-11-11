
fn test() -> bool {
    if true {
        return true
    } else {
        return false
    }
}

fn test2() -> bool {
    return false
}


fn main() {
    println!("Hello, world!");
    println!("{}", test());
    println!("{}", test2());
}
