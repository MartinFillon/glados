//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// mod
//

use std::collections::VecDeque;

use logical::{op_and, op_eq, op_gt, op_lt, op_neq, op_not, op_or};
use mathematical::{op_add, op_div, op_mod, op_mul, op_sub};

use crate::instructions::Value;

mod logical;
mod mathematical;

pub enum Operators {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    And,
    Or,
    Gt,
    Lt,
    Not,
}

impl Operators {
    pub fn exec(op: Operators, stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
        match op {
            Operators::Add => op_add(stack),
            Operators::Sub => op_sub(stack),
            Operators::Mul => op_mul(stack),
            Operators::Div => op_div(stack),
            Operators::Mod => op_mod(stack),
            Operators::Eq => op_eq(stack),
            Operators::Neq => op_neq(stack),
            Operators::And => op_and(stack),
            Operators::Or => op_or(stack),
            Operators::Gt => op_gt(stack),
            Operators::Lt => op_lt(stack),
            Operators::Not => op_not(stack),
        }
    }
}

impl TryFrom<String> for Operators {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "add" => Ok(Operators::Add),
            "sub" => Ok(Operators::Sub),
            "div" => Ok(Operators::Div),
            "mul" => Ok(Operators::Mul),
            "mod" => Ok(Operators::Mod),
            "not" => Ok(Operators::Not),
            "eq" => Ok(Operators::Eq),
            "neq" => Ok(Operators::Neq),
            "or" => Ok(Operators::Or),
            "and" => Ok(Operators::And),
            "great" => Ok(Operators::Gt),
            "less" => Ok(Operators::Lt),
            _ => Err(format!("Unknown operator")),
        }
    }
}
