// my/mod.rs
// Similarily `mod inaccesible` and `mod nested` will locate the `nested.rs`
// and `inaccessible.rs` files and insert them here under their respecitve
// modules
mod hidden;      // cannot be accessed from outside because it is not public
pub mod nested;  // notice that this is a public module


pub fn function() {
    println!("called my::function()");
}

fn private_function() {
    println!("Called my:private_function()");
}

pub fn indirect_access() {
    print!("Called my::indirect access that ");
    private_function();
}
