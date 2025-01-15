//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// list
//

use std::collections::VecDeque;

use crate::instructions::Value;

pub fn op_get(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let idx = stack.pop_front().ok_or("Missing Arguments")?;
    let lst = stack.pop_front().ok_or("Missing Arguments")?;

    if let (Value::Int(i), Value::Array(l)) = (idx, lst) {
        let v = l.get(i as usize).ok_or("Index out of bound")?;

        stack.push_front(v.clone());
        Ok(stack)
    } else {
        Err(format!("Bad argument"))
    }
}

pub fn op_set(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let v = stack.pop_front().ok_or("Missing Arguments")?;
    let idx = stack.pop_front().ok_or("Missing Arguments")?;
    let lst = stack.pop_front().ok_or("Missing Arguments")?;

    if let (Value::Int(i), Value::Array(mut l)) = (idx, lst) {
        l[i as usize] = v;
        stack.push_front(Value::Array(l));
        Ok(stack)
    } else {
        Err(format!("Bad argument"))
    }
}

pub fn op_push(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let v = stack.pop_front().ok_or("Missing Arguments")?;
    let lst = stack.pop_front().ok_or("Missing Arguments")?;

    if let Value::Array(mut l) = lst {
        l.push(v);
        stack.push_front(Value::Array(l));
        Ok(stack)
    } else {
        Err(format!("Bad argument"))
    }
}

pub fn op_pop(mut stack: VecDeque<Value>) -> Result<VecDeque<Value>, String> {
    let idx = stack.pop_front().ok_or("Missing Arguments")?;
    let lst = stack.pop_front().ok_or("Missing Arguments")?;

    if let (Value::Int(i), Value::Array(mut l)) = (idx, lst) {
        l.remove(i as usize);
        stack.push_front(Value::Array(l));
        Ok(stack)
    } else {
        Err(format!("Bad argument"))
    }
}
