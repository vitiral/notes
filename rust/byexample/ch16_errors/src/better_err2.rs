use std;
use std::fmt;
use std::num::ParseIntError;

type Result<T> = std::result::Result<T, DoubleError>;

#[derive(Debug)]
// define error types
enum DoubleError {
    // no extra info needed here!
    EmptyVec,
    // we will defer to the parse error implementation for their error
    // supplying extra info would require adding more data to the
    // datatype
    Parse(ParseIntError),
}

impl From<ParseIntError> for DoubleError {
    fn from(err: ParseIntError) -> DoubleError {
        DoubleError::Parse(err)
    }
}

impl fmt::Display for DoubleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DoubleError::EmptyVec => write!(f, "please use a vector with at least one element"),
            DoubleError::Parse(ref e) => e.fmt(f),
        }
    }
}

fn double_first(vec: Vec<&str>) -> Result<i32> {
    let first = try!(vec.first().ok_or(DoubleError::EmptyVec));
    let value = try!(first.parse::<i32>());
    Ok(value * 2)
}

fn print(result: Result<i32>) {
    match result {
        Ok(n) => println!("the first doubled is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}

pub fn function() {
    println!("# Learning better error creation");
    let numbers = vec!["93", "18"];
    let empty: Vec<&str> = vec![];
    let strs = vec!["tofu", "93"];
    print(double_first(numbers));
    print(double_first(empty));
    print(double_first(strs));

}
