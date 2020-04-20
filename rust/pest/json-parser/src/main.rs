extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::collections::HashMap;
use std::fs;
use pest::Parser;
use pest::iterators::Pair;


#[derive(Parser)]
#[grammar = "json.pest"]
pub struct JSONParser;

enum JSONValue<'a> {
    Object(Vec<(&'a str, JSONValue<'a>)>),
    Array(Vec<JSONValue<'a>>),
    String(&'a str),
    Number(f64),
    Boolean(bool),
    Null,
}

fn main() {
    // let unparsed_file = fs::read_to_string("example.json").expect("cannot read file");
    let unparsed_file = fs::read_to_string("example.json").expect("cannot read file");
    
    let pair = JSONParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse")
        .next()
        .unwrap();
    let json = parse_pair(pair);
    println!("{}", serialize_jsonvalue(&json));
}

fn parse_pair(pair: Pair<Rule>) -> JSONValue {
    match pair.as_rule() {
        Rule::object => JSONValue::Object(
            pair.into_inner()
              .map(|pair| {
                  let mut inner_rules = pair.into_inner();
                  let key = inner_rules
                      .next()
                      .unwrap() // string
                      .into_inner()
                      .next()
                      .unwrap() // string_inner
                      .as_str();
                  let value = parse_pair(inner_rules.next().unwrap());
                  (key, value)
              })
              .collect()
        ),
        Rule::array => JSONValue::Array(pair.into_inner().map(parse_pair).collect()),
        Rule::string => JSONValue::String(
            pair
            .into_inner()
            .next()
            .unwrap() // string_inner
            .as_str()
        ),
        Rule::number => JSONValue::Number(
            pair.as_str().parse::<f64>().unwrap()
        ),
        Rule::boolean => JSONValue::Boolean(pair.as_str().parse().unwrap()),
        Rule::null => JSONValue::Null,
        _ => panic!(),
    }
}

fn serialize_jsonvalue(val: &JSONValue) -> String {
    use JSONValue::*;

    match val {
        Object(mappings) => {
            let contents: Vec<_> = mappings
                .iter()
                .map(|(name, value)| format!("\"{}\":{}", name, serialize_jsonvalue(value)))
                .collect();
            format!("{{{}}}", contents.join(","))
        }
        Array(values) => {
            let contents: Vec<_> = values
                .iter()
                .map(serialize_jsonvalue)
                .collect();
            format!("[{}]", contents.join(","))
        }
        String(s) => format!("\"{}\"", s),
        Number(n) => format!("{}", n),
        Boolean(b) => format!("{}", b),
        Null => "null".to_owned(),
    }
}
