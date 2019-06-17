extern crate pest;
extern crate pest_derive;

use std::fs;

mod builtins;
mod model;
mod parser;

fn main() {
    let builtin_types = builtins::parse_types();

    let data = fs::read_to_string("example.widl").expect("cannot read file");

    let module = parser::parse(&data).expect("parse failed");

    println!("{:?}", module)
}
