// rary.rs library
#![crate_type = "lib"]
#![crate_name = "rary"]

pub fn public_function() {
    println!("called rary's public_function()");
}

#[cfg(target_os = "linux")]
pub fn on_linux() {
    println!("You are on linux!");
}

#[cfg(not(target_os = "linux"))]
pub fn on_linux() {
    println!("You NOT are on linux!");
}

