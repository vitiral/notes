extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::collections::HashMap;
use std::fs;
use pest::Parser;

#[derive(Parser)]
#[grammar = "ini.pest"]
pub struct INIParser;

fn main() {
    let unparsed_file = fs::read_to_string("example.ini").expect("cannot read file");
    
    let file = INIParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse")
        .next().unwrap(); // get and unwrap "file" rule, never fails.

    let mut properties: HashMap<&str, HashMap<&str, &str>> = HashMap::new();

    let mut current_section_name = "";

    for pair in file.into_inner() {
        match pair.as_rule() {
            Rule::section => {
                current_section_name = pair.into_inner().next().unwrap().as_str();
            },
            Rule::property => {
                let mut inner = pair.into_inner();

                let name = inner.next().unwrap().as_str();
                let value = inner.next().unwrap().as_str();

                let section = properties.entry(current_section_name).or_default();
                section.insert(name, value);
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    println!("Properties: {:#?}", properties);
}
