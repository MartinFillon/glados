//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// main
//

#![allow(dead_code)]

use std::process::exit;

use execute::State;

mod execute;
mod instructions;
mod parser;
mod operators;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let mut insts = Vec::new();

    for arg in args.iter().skip(1) {
        let content = std::fs::read_to_string(arg).unwrap();
        let mut parser = parser::Parser::new(&content);
        let instructions = parser.parse().unwrap();

        for inst in instructions {
            insts.push(inst);
        }
    }

    let mut s = State::new(insts, vec![]);

    let r = s
        .run()
        .map_err(|e| {
            eprintln!("{e}");
            exit(84)
        })
        .unwrap();
    dbg!(r);
}
