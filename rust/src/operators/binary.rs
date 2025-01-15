//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// binary
//

use std::collections::VecDeque;

use crate::instructions::Value;

pub fn op_band(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na & nb),
        _ => return Err(format!("Invalid binary Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_bor(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na | nb),
        _ => return Err(format!("Invalid binary Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_xor(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na ^ nb),
        _ => return Err(format!("Invalid binary Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_shiftr(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na >> nb),
        _ => return Err(format!("Invalid binary Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_shiftl(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na << nb),
        _ => return Err(format!("Invalid binary Op")),
    };

    stack.push_front(r);
    Ok(stack)
}
