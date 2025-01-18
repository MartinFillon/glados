//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// instructions
//

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i32),
    Double(f64),
    String(String),
    Array(Vec<Value>),
    Char(char),
    Bool(bool),
    Object(Vec<(String, Value)>),
}

#[derive(Debug, Clone)]
pub enum JumpValue {
    Index(i32),
    Label(String),
}

#[derive(Debug, Clone)]
pub enum Insts {
    Push(Value),
    Ret,
    Call(String),
    PushArg(i32),
    Jump(JumpValue),
    JumpIfFalse(JumpValue),
    Dup,
    Void,
}

#[derive(Debug, Clone)]
pub struct Instructions {
    pub inst: Insts,
    pub label: Option<String>,
}

pub fn push(value: Value, label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::Push(value),
        label,
    }
}

pub fn ret(label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::Ret,
        label,
    }
}

pub fn call(name: String, label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::Call(name),
        label,
    }
}

pub fn push_arg(index: i32, label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::PushArg(index),
        label,
    }
}

pub fn jump(value: JumpValue, label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::Jump(value),
        label,
    }
}

pub fn jump_if_false(value: JumpValue, label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::JumpIfFalse(value),
        label,
    }
}

pub fn dup(label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::Dup,
        label,
    }
}

pub fn void(label: Option<String>) -> Instructions {
    Instructions {
        inst: Insts::Void,
        label,
    }
}
