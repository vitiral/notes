// executable.rs
// Link to the library, import items under the rary module
extern crate rary;

#[cfg(some_condition)]
fn conditional_function() {
    println!("condition met!");
}

fn main() {
    rary::public_function();
    rary::on_linux();
    if cfg!(target_os = "linux") {
        println!("Yup, linux");
    } else {
        println!("NOPE, you aren't on linux dummy");
    }
    conditional_function();
}
