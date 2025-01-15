//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// mathematical
//

use std::collections::VecDeque;

use crate::instructions::Value;

pub fn op_add(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na + nb),
        (Value::Double(na), Value::Double(nb)) => Value::Double(na + nb),
        (Value::Int(na), Value::Double(nb)) => Value::Double(na as f64 + nb),
        (Value::Double(na), Value::Int(nb)) => Value::Double(na + nb as f64),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_sub(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na - nb),
        (Value::Double(na), Value::Double(nb)) => Value::Double(na - nb),
        (Value::Int(na), Value::Double(nb)) => Value::Double(na as f64 - nb),
        (Value::Double(na), Value::Int(nb)) => Value::Double(na - nb as f64),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_mul(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(na), Value::Int(nb)) => Value::Int(na * nb),
        (Value::Double(na), Value::Double(nb)) => Value::Double(na * nb),
        (Value::Int(na), Value::Double(nb)) => Value::Double(na as f64 * nb),
        (Value::Double(na), Value::Int(nb)) => Value::Double(na * nb as f64),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_div(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (_, Value::Int(0)) => return Err(format!("Division by zero")),
        (_, Value::Double(0.0)) => return Err(format!("Division by zero")),
        (Value::Int(na), Value::Int(nb)) => Value::Int(na / nb),
        (Value::Double(na), Value::Double(nb)) => Value::Double(na / nb),
        (Value::Int(na), Value::Double(nb)) => Value::Double(na as f64 / nb),
        (Value::Double(na), Value::Int(nb)) => Value::Double(na / nb as f64),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}

pub fn op_mod(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let b = stack.pop_front().ok_or("Missing argument")?;
    let a = stack.pop_front().ok_or("Missing argument")?;

    let r = match (a, b) {
        (Value::Int(_), Value::Int(0)) => return Err(format!("Modulo by zero")),
        (Value::Int(na), Value::Int(nb)) => Value::Int(na % nb),
        _ => return Err(format!("Invalid Numeric Op")),
    };

    stack.push_front(r);
    Ok(stack)
}
