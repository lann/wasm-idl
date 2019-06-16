extern crate pest;
extern crate pest_derive;

use std::fs;

mod parser;
mod builtins;

fn main() {
    for builtin in builtins::parse_types() {
        println!("{:?}", builtin);
    }

    let data = fs::read_to_string("example.widl").expect("cannot read file");

    let module = parser::parse(&data).expect("parse failed");

    // println!("{:?}", module) 
}
