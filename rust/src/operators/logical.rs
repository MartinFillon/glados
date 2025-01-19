//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// logical
//

use std::collections::VecDeque;

use crate::instructions::Value;

pub fn op_eq(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    stack.push_front(Value::Bool(a == b));
    Ok(stack)
}

pub fn op_neq(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    stack.push_front(Value::Bool(a != b));
    Ok(stack)
}

pub fn op_and(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Bool(va), Value::Bool(vb)) => Value::Bool(va && vb),
        _ => return Err(format!("And expects two booleans")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_or(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Bool(va), Value::Bool(vb)) => Value::Bool(va || vb),
        _ => return Err(format!("Or expects two booleans")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_not(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match a {
        Value::Bool(vb) => Value::Bool(!vb),
        _ => return Err(format!("And expects two booleans")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_gt(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Bool(na > nb),
        (Value::Double(na), Value::Double(nb)) => Value::Bool(na > nb),
        (Value::Int(na), Value::Double(nb)) => Value::Bool(na as f64 > nb),
        (Value::Double(na), Value::Int(nb)) => Value::Bool(na > nb as f64),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_lt(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Bool(na < nb),
        (Value::Double(na), Value::Double(nb)) => Value::Bool(na < nb),
        (Value::Int(na), Value::Double(nb)) => Value::Bool((na as f64) < nb),
        (Value::Double(na), Value::Int(nb)) => Value::Bool(na < nb as f64),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}
