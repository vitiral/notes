use std::io::prelude::*;
use std::num::ParseIntError;

fn get_line_dimensions(s: &str) -> Result<(i32, i32, i32), String> {
    let mut dimensions = Vec::new();
    for val in s.split("x") {
        dimensions.push(match val.parse::<i32>() {
            Ok(n) => n,
            Err(_) => return Err("couldn't parse int".to_string()),
        })
    }
    match dimensions.len() {
        3 => {
            let d = &dimensions;
            return Ok((d[0], d[1], d[2]))
        }
        _ => {
            let msg = format!("wrong num of dimensions: {}", dimensions.len());
            return Err(msg);
            // return Err("wrong number of dimensions");
        },
    }
}

#[test]
fn test_get_line_dimensions() {
    assert_eq!((1,2,3), get_line_dimensions("1x2x3").unwrap());
    assert_eq!((6, 3, 2), get_line_dimensions("6x3x2").unwrap());
    assert_eq!((42, 8, 12342), get_line_dimensions("42x8x12342").unwrap());
    assert!(get_line_dimensions("1x2").is_err());      // not enough dimensions
    assert!(get_line_dimensions("1x2x3x4").is_err());  // too many dimensions
    assert!(get_line_dimensions("1x4$x3").is_err());   // cannot parse int
}

fn get_dimensions_from_file(f: &std::io::BufRead) -> Result<Vec<(i32, i32, i32)>, String>{
    let mut dimensions = Vec::new();
    for l in f.lines() {
        dimensions.push(try!(get_line_dimensions(&*l.unwrap())));
    }
    Ok(dimensions)
}

#[test]
fn test_get_dimensions_from_file() {
    let buf = std::io::BufRead::new("1x2x3\n2x3x4");
}

fn main() {
    println!("builds");
}
