//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// execute
//

use std::collections::VecDeque;

use crate::{
    instructions::{jump, Instructions, Insts, JumpValue, Value},
    operators::Operators,
};

#[derive(Debug)]
pub struct State {
    stack: VecDeque<Value>,
    args: Vec<Value>,
    pc: i32,
    insts: Vec<Instructions>,
}

impl State {
    pub fn new(parsed_insts: Vec<Instructions>, args: Vec<Value>) -> Self {
        let mut insts = vec![jump(JumpValue::Label(String::from(".start")), None)];

        insts = [insts, parsed_insts].concat();

        Self {
            stack: VecDeque::with_capacity(100),
            args,
            pc: 0,
            insts,
        }
    }

    pub fn run(&mut self) -> Result<Option<Value>, String> {
        let mut last_result = None;

        while (self.pc as usize) < self.insts.len() && self.pc >= 0 {
            last_result = match self.insts.get(self.pc as usize) {
                None => Err(format!("No instructions at pc")),
                Some(Instructions {
                    inst: Insts::Ret,
                    label: _,
                }) => match self.stack.pop_front() {
                    Some(v) => return Ok(Some(v)),
                    None => Err(format!("err")),
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

                Some(Instructions {
                    inst: Insts::Noop,
                    label: _,
                }) => Ok(None),
            }?;
            self.pc += 1;
        }

        last_result
            .ok_or(format!("Last instruction didnt yield a value."))
            .map(Some)
    }

    fn exec_jump(&mut self, jv: JumpValue) -> Result<Option<Value>, String> {
        match jv {
            JumpValue::Index(n) if n > 0 => self.pc += n - 1,
            JumpValue::Index(n) => self.pc += n,
            JumpValue::Label(s) => {
                self.pc = self
                    .get_instruction_idx_at_label(s.clone())
                    .ok_or(format!("Label {s} not found"))? as i32
                    - 1
            }
        }
        Ok(None)
    }

    fn top(&mut self) -> Result<Value, String> {
        self.stack.pop_front().ok_or(format!("No values on stack"))
    }

    fn exec_jumpf(&mut self, jv: JumpValue) -> Result<Option<Value>, String> {
        let top = self.top()?;
        match top {
            Value::Bool(false) => {
                match jv {
                    JumpValue::Index(n) if n > 0 => self.pc += n - 1,
                    JumpValue::Index(n) => self.pc += n,
                    JumpValue::Label(s) => {
                        self.pc = self
                            .get_instruction_idx_at_label(s.clone())
                            .ok_or(format!("Label {s} not found"))?
                            as i32
                            - 1
                    }
                }
                Ok(None)
            }
            Value::Bool(true) => Ok(None),
            _ => Err(format!("Top of the stack is not a boolean")),
        }
    }

    fn get_instruction_idx_at_label(&self, label: String) -> Option<usize> {
        self.insts
            .iter()
            .position(|ins| ins.label == Some(label.clone()))
    }

    fn exec_dup(&mut self) -> Result<Option<Value>, String> {
        let v = self.top()?;

        self.stack.push_front(v.clone());
        self.stack.push_front(v);

        Ok(None)
    }

    fn exec_pusharg(&mut self, n: i32) -> Result<Option<Value>, String> {
        self.stack.push_front(
            self.args
                .get(n as usize)
                .ok_or(format!("No arg at index {n}"))?
                .clone(),
        );

        Ok(None)
    }

    fn exec_call(&mut self, f: String) -> Result<Option<Value>, String> {
        if let Ok(o) = f.clone().try_into() {
            self.stack = Operators::exec(o, self.stack.clone())?;
            Ok(None)
        } else if let Some(lbl) = self.get_instruction_idx_at_label(f.clone()) {
            let mut nv = Self {
                stack: VecDeque::new(),
                args: self.stack.iter().rev().cloned().collect(),
                pc: lbl as i32,
                insts: self.insts.clone(),
            };

            let v = nv.run()?.ok_or("Missing last value to call")?;
            self.stack.push_front(v);
            Ok(None)
        } else {
            Err(format!("Function {f} Not Found"))
        }
    }
}
