
use std::path;
use std::borrow::Cow;
use std::result::Result::{self, Ok, Err};

fn enum_variant(v: bool) -> Result<(), ()> {
    if v {
        Ok(())
    } else {
        Err(())
    }
}

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

#[test]
fn testit() {
    let p = path::Path::new("path");
    assert_eq!(p.join("foo"), path::Path::new("path/foo"));

    let path = path::Path::new("/oh/hello/there/bob");
    // identical whether you strip the trailing `/`
    assert_eq!(path.strip_prefix("/oh/hello").unwrap(), path::Path::new("there/bob"));
    assert_eq!(path.strip_prefix("/oh/hello/").unwrap(), path::Path::new("there/bob"));

    match 'z' {
        'a' ... 'z' => {},
        _ => panic!("agh"),
    };

    // Cow shenanigans
    let mut cow: Cow<[_]> = Cow::Owned(vec![1,2,3]);
    {
        let hello = cow.to_mut();
        assert_eq!(hello, &[1, 2, 3]);
    }

    let v = Vec::from(cow);
    assert_eq!(v, &[1, 2, 3]);

    assert_eq!(enum_variant(true), Ok(()));
    assert_eq!(enum_variant(false), Err(()));
}

fn main() {
    println!("Hello, world!");
    println!("{}", test());
    println!("{}", test2());
}
