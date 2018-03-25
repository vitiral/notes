#[macro_use]
extern crate ordermap;

use std::collections::HashMap;

#[test]
fn test_it() {
    let key = "foo".to_string();
    let mut m = ordermap!{key.clone() => 3};
    assert!(m.contains_key(&&key));

    let mut h = HashMap::new();
    h.insert(key.clone(), 3);
    assert!(h.contains_key(&&key));

}

fn main() {
    println!("yay");
}
