//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// parser
//

use std::{iter::Peekable, str::Chars};

use crate::instructions::{
    call, dup, jump, jump_if_false, push, push_arg, ret, void, Instructions, JumpValue, Value,
};

#[derive(Debug)]
pub struct Parser<'a> {
    current: Peekable<Chars<'a>>,
    buf: String,
    instructions: Vec<Instructions>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(Option<char>),
    UnexpectedEnd,
    NotANumber(String),
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            current: input.chars().peekable(),
            buf: String::new(),
            instructions: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Instructions>, ParseError> {
        while self.current.peek().is_some() {
            self.skip_whitespace();
            let label = self.parse_label()?;
            self.skip_whitespace();
            if self.r#match("pushArg")? {
                self.parse_push_arg(label)
            } else if self.r#match("push")? {
                self.parse_push(label)
            } else if self.r#match("ret")? {
                self.parse_ret(label)
            } else if self.r#match("dup")? {
                self.parse_dup(label)
            } else if self.r#match("void")? {
                self.parse_void(label)
            } else if self.r#match("call")? {
                self.parse_call(label)
            } else if self.r#match("jumpf")? {
                self.parse_jumpf(label)
            } else if self.r#match("jump")? {
                self.parse_jump(label)
            } else {
                Err(ParseError::UnexpectedChar(self.current.next()))
            }?;
            self.current.next();
        }
        Ok(self.instructions.clone())
    }

    fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.current.next();
        }
    }

    fn consume(&mut self, to_match: &'static str) -> Result<(), ParseError> {
        if self.r#match(to_match)? {
            self.skip(to_match.len());
            Ok(())
        } else {
            Err(ParseError::UnexpectedChar(self.current.next()))
        }
    }

    fn parse_push(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("push")?;
        let v = self.parse_value()?;
        self.instructions.push(push(v, label));
        Ok(())
    }

    fn parse_value(&mut self) -> Result<Value, ParseError> {
        self.skip_whitespace();
        match self.current.peek() {
            Some('0'..='9') => self.parse_number(),
            Some('\'') => self.parse_char(),
            Some('\"') => self.parse_string(),
            _ => Err(ParseError::UnexpectedChar(self.current.next())),
        }
    }

    fn parse_str(&mut self) -> Result<String, ParseError> {
        self.consume("\"")?;
        let mut buf = String::new();

        while let Some(&c) = self.current.peek() {
            match c {
                '\\' => {
                    self.current.next();
                    match self.current.next() {
                        Some('n') => buf.push('\n'),
                        Some('t') => buf.push('\t'),
                        Some('r') => buf.push('\r'),
                        Some('\\') => buf.push('\\'),
                        Some('\'') => buf.push('\''),
                        Some('\"') => buf.push('\"'),
                        Some(c) => return Err(ParseError::UnexpectedChar(Some(c))),
                        None => return Err(ParseError::UnexpectedEnd),
                    }
                }
                '\"' => {
                    self.current.next();
                    break;
                }
                _ => {
                    buf.push(c);
                    self.current.next();
                }
            }
        }
        Ok(buf)
    }

    fn parse_string(&mut self) -> Result<Value, ParseError> {
        Ok(Value::String(self.parse_str()?))
    }

    fn parse_char(&mut self) -> Result<Value, ParseError> {
        self.consume("\'")?;
        let c = match self.current.next().ok_or(ParseError::UnexpectedEnd)? {
            '\\' => {
                self.current.next();
                match self.current.next() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('r') => '\r',
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('\"') => '\"',
                    Some(c) => return Err(ParseError::UnexpectedChar(Some(c))),
                    None => return Err(ParseError::UnexpectedEnd),
                }
            }
            c => c,
        };
        self.consume("\'")?;
        Ok(Value::Char(c))
    }

    fn parse_number(&mut self) -> Result<Value, ParseError> {
        let mut number = String::new();
        let mut decimal = false;

        while let Some(&c) = self.current.peek() {
            match c {
                '-' if number.is_empty() => {
                    number.push(c);
                    self.current.next();
                }
                '0'..='9' => {
                    number.push(c);
                    self.current.next();
                }
                '.' => {
                    number.push(c);
                    if decimal {
                        return Err(ParseError::NotANumber(number));
                    }
                    decimal = true;
                    self.current.next();
                }
                ca if ca.is_whitespace() => break,
                ca => {
                    number.push(ca);
                    return Err(ParseError::NotANumber(number));
                }
            }
        }
        if decimal {
            Ok(Value::Double(
                number.parse().map_err(|_| ParseError::NotANumber(number))?,
            ))
        } else {
            Ok(Value::Int(
                number.parse().map_err(|_| ParseError::NotANumber(number))?,
            ))
        }
    }

    fn r#match(&mut self, to_match: &'static str) -> Result<bool, ParseError> {
        let mut copy = self.current.clone();

        for c in to_match.chars() {
            match copy.next() {
                Some(c2) if c == c2 => (),
                _ => return Ok(false),
            }
        }

        Ok(true)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.current.peek() {
            if c.is_whitespace() {
                self.current.next();
            } else {
                break;
            }
        }
    }

    fn parse_ret(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("ret")?;
        self.instructions.push(ret(label));
        Ok(())
    }

    fn parse_dup(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("dup")?;
        self.instructions.push(dup(label));
        Ok(())
    }

    fn parse_void(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("void")?;
        self.instructions.push(void(label));
        Ok(())
    }

    fn parse_label(&mut self) -> Result<Option<String>, ParseError> {
        if self.current.peek() == Some(&'.') {
            self.skip_whitespace();
            let mut label = String::new();
            while let Some(&c) = self.current.peek() {
                if c.is_whitespace() {
                    break;
                }
                label.push(c);
                self.current.next();
            }
            Ok(Some(label))
        } else {
            Ok(None)
        }
    }

    fn parse_call(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("call")?;
        self.skip_whitespace();
        let arg = self.parse_str()?;
        self.instructions.push(call(arg, label));
        Ok(())
    }

    fn parse_int(&mut self) -> Result<i32, ParseError> {
        let mut number = String::new();

        while let Some(&c) = self.current.peek() {
            match c {
                '-' if number.is_empty() => {
                    number.push(c);
                    self.current.next();
                }
                '0'..='9' => {
                    number.push(c);
                    self.current.next();
                }
                ca if ca.is_whitespace() => break,
                ca => {
                    number.push(ca);
                    return Err(ParseError::NotANumber(number));
                }
            }
        }
        number.parse().map_err(|_| ParseError::NotANumber(number))
    }

    fn parse_push_arg(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("pushArg")?;
        self.skip_whitespace();
        let arg = self.parse_int()?;
        self.instructions.push(push_arg(arg, label));
        Ok(())
    }

    fn parse_jump_val(&mut self) -> Result<JumpValue, ParseError> {
        match self.current.peek() {
            Some('0'..='9') => Ok(JumpValue::Index(self.parse_int()?)),
            Some('\"') => Ok(JumpValue::Label(self.parse_str()?)),
            _ => Err(ParseError::UnexpectedChar(self.current.next())),
        }
    }

    fn parse_jump(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("jump")?;
        self.skip_whitespace();
        let arg = self.parse_jump_val()?;
        self.instructions.push(jump(arg, label));
        Ok(())
    }

    fn parse_jumpf(&mut self, label: Option<String>) -> Result<(), ParseError> {
        self.consume("jumpf")?;
        self.skip_whitespace();
        let arg = self.parse_jump_val()?;
        self.instructions.push(jump_if_false(arg, label));
        Ok(())
    }
}
