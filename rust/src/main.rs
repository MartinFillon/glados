//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// main
//

#![allow(dead_code)]

mod instructions;
mod parser;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    for arg in args.iter().skip(1) {
        let content = std::fs::read_to_string(arg).unwrap();
        let mut parser = parser::Parser::new(&content);
        let instructions = parser.parse().unwrap();
        dbg!(&instructions);
        for inst in instructions {
            println!("{:?}", inst);
        }
    }
}
