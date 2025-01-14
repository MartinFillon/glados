//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// execute
//

use std::collections::VecDeque;

use crate::instructions::{jump, Instructions, Insts, JumpValue, Value};

#[derive(Debug)]
pub struct State {
    stack: VecDeque<Value>,
    args: Vec<Value>,
    pc: i32,
    insts: Vec<Instructions>,
}

impl State {
    pub fn new(parsed_insts: Vec<Instructions>, args: Vec<Value>) -> Self {
        let mut insts = Vec::new();

        insts.push(jump(JumpValue::Label(String::from(".start")), None));

        insts = [insts, parsed_insts].concat();

        Self {
            stack: VecDeque::with_capacity(100),
            args,
            pc: 0,
            insts,
        }
    }

    pub fn run(&mut self) -> Result<Option<Value>, &'static str> {
        let mut last_result = None;

        while (self.pc as usize) < self.insts.len() && self.pc >= 0 {
            last_result = match self.insts.get(self.pc as usize) {
                None => Err("No instructions at pc"),
                Some(Instructions {
                    inst: Insts::Ret,
                    label: _,
                }) => match self.stack.pop_front() {
                    Some(v) => Ok(Some(v)),
                    None => Err("err"),
                },

                Some(Instructions {
                    inst: Insts::Push(v),
                    label: _,
                }) => {
                    self.stack.push_front(v.clone());
                    Ok(None)
                }

                Some(Instructions {
                    inst: Insts::Jump(jv),
                    label: _,
                }) => self.exec_jump(jv.clone()),

                Some(Instructions {
                    inst: Insts::JumpIfFalse(jv),
                    label: _,
                }) => self.exec_jumpf(jv.clone()),

                Some(Instructions {
                    inst: Insts::Void,
                    label: _,
                }) => {
                    let _ = self.top();
                    Ok(None)
                }

                Some(Instructions {
                    inst: Insts::Dup,
                    label: _,
                }) => self.exec_dup(),

                Some(Instructions {
                    inst: Insts::Call(f),
                    label: _,
                }) => self.exec_call(f.clone()),

                Some(Instructions {
                    inst: Insts::PushArg(n),
                    label: _,
                }) => self.exec_pusharg(*n),
            }?;
            self.pc += 1;
        }

        last_result
            .ok_or("Last instruction didnt yield a value.")
            .map(|x| Some(x))
    }

    fn exec_jump(&mut self, jv: JumpValue) -> Result<Option<Value>, &'static str> {
        match jv {
            JumpValue::Index(n) if n > 0 => self.pc += n - 1,
            JumpValue::Index(n) => self.pc += n,
            JumpValue::Label(s) => {
                self.pc = self
                    .get_instruction_idx_at_label(s)
                    .ok_or("label not found")? as i32
                    - 1
            }
        }
        Ok(None)
    }

    fn top(&mut self) -> Result<Value, &'static str> {
        self.stack.pop_front().ok_or("No values on stack")
    }

    fn exec_jumpf(&mut self, jv: JumpValue) -> Result<Option<Value>, &'static str> {
        let top = self.top()?;
        match top {
            Value::Bool(false) => (),
            Value::Bool(true) => return Ok(None),
            _ => return Err("Top of the stack is not a boolean"),
        };
        match jv {
            JumpValue::Index(n) if n > 0 => self.pc += n - 1,
            JumpValue::Index(n) => self.pc += n,
            JumpValue::Label(s) => {
                self.pc = self
                    .get_instruction_idx_at_label(s)
                    .ok_or("label not found")? as i32
                    - 1
            }
        }
        Ok(None)
    }

    fn get_instruction_idx_at_label(&self, label: String) -> Option<usize> {
        self.insts
            .iter()
            .position(|ins| ins.label == Some(label.clone()))
    }

    fn exec_dup(&mut self) -> Result<Option<Value>, &'static str> {
        let v = self.top()?;

        self.stack.push_front(v.clone());
        self.stack.push_front(v);

        Ok(None)
    }

    fn exec_pusharg(&mut self, n: i32) -> Result<Option<Value>, &'static str> {
        self.stack
            .push_front(self.args.get(n as usize).ok_or("No arg at index")?.clone());

        Ok(None)
    }

    fn exec_call(&mut self, f: String) -> Result<Option<Value>, &'static str> {
        todo!("Exec {f}");
    }
}
