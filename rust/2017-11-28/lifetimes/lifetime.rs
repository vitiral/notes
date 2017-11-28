extern crate regex;

use regex::Regex;

#[derive(Debug)]
struct Field {
    name: &'static str,
    date: &'static str,
    record: &'static str,
}

struct MyErr;

type Result<T> = std::result::Result<T, MyErr>;

fn parse_captured(text: &'static str, cap: &regex::Captures) -> Result<Field> {
    let pos = cap.pos(0).unwrap();
    Ok(Field {
        name: cap.at(1).unwrap(),
        date: cap.at(2).unwrap(),
        record: &text[pos.1..pos.1 + 5],
    })
}

fn parse_records(text: &'static str) -> Result<Vec<Field>> {
    let mut out: Vec<Field> = vec!();
    let re = Regex::new(r"\(\w):(\d{4})-(\d{2})-(\d{2})").unwrap();
    for cap in re.captures_iter(text) {
        out.push(try!(parse_captured(text, &cap, ('{', '}'))));
    }
    Ok(out)
}


