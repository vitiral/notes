use std::io::prelude::*;
use std::fs::File;

/// find_floor finds the floor that santa sould go to.
/// '(' means up one floor and ')' means down one
fn find_floor(text: &str, end: Option<i32>) -> Option<i32> {
    let mut floor = 0;
    let mut pos = 1;
    for c in text.chars() {
        match c {
            '(' => floor += 1,
            ')' => floor -= 1,
            '\n' => {},
            _ => panic!("unknown character {}", c),
        }
        match end {
            Some(f) => {
                if f == floor {return Some(pos);}
            },
            _ => {},
        }
        pos += 1;
    }
    match end {
        Some(_) => return None,  // error, never found requested floor
        None => return Some(floor),
    }
}

#[test]
fn test_find_floor() {
    assert_eq!(0, find_floor("(())", None).unwrap());
    assert_eq!(0, find_floor("()()", None).unwrap());
    assert_eq!(3, find_floor("(()(()(", None).unwrap());
    assert_eq!(3, find_floor("))(((((", None).unwrap());

    assert_eq!(5, find_floor("(()))", Some(-1)).unwrap())
}

fn main(){
    let mut f = File::open("data/directions.txt").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    println!("puzzle 1 answer: {}", find_floor(&s, None).unwrap());
    // let basement : Option<i32> = -1;
    println!("puzzle 2 answer: {}", find_floor(&s, Some(-1)).unwrap());
}
