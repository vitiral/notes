extern crate rustc_serialize;
extern crate toml;

use toml::{encode_str, Value};

#[derive(RustcEncodable)]
struct MyStruct { foo: isize, bar: String }

const LONG_TEXT: &'static str = r#"
this is some text
here is some more
yay
"#;

fn main() {
    let my_struct = MyStruct { foo: 4, bar: LONG_TEXT.to_string() };
    println!("{}", encode_str(&my_struct));

    let mystr = Value::String(String::from("hi there"));
    println!("toml::String: {}", mystr);

    if let Value::String(ref s) = mystr {
        println!("unpacked String: {}", s);
    } else {
        unreachable!();
    }
}
