//
// EPITECH PROJECT, 2025
// gladdos
// File description:
// parser
//

use std::{iter::Peekable, str::Chars};

use crate::instructions::{push, Instructions, Value};

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
        while let Some(_) = self.current.peek() {
            self.skip_whitespace();
            if self.r#match("push")? {
                dbg!("push");
                self.parse_push()?;
            }
            self.current.next();
        }
        Ok(self.instructions.clone())
    }

    pub fn skip(&mut self, n: usize) -> () {
        for _ in 0..n {
            self.current.next();
        }
        ()
    }

    pub fn consume(&mut self, to_match: &'static str) -> Result<(), ParseError> {
        if self.r#match(to_match)? {
            self.skip(to_match.len());
            Ok(())
        } else {
            Err(ParseError::UnexpectedChar(self.current.next()))
        }
    }

    pub fn parse_push(&mut self) -> Result<(), ParseError> {
        self.consume("push")?;
        let v = self.parse_value()?;
        self.instructions.push(push(v, None));
        Ok(())
    }

    pub fn parse_value(&mut self) -> Result<Value, ParseError> {
        self.skip_whitespace();
        match self.current.peek() {
            Some('0'..='9') => self.parse_number(),
            Some('\'') => self.parse_char(),
            Some('\"') => self.parse_string(),
            _ => Err(ParseError::UnexpectedChar(self.current.next())),
        }
    }

    pub fn parse_string(&mut self) -> Result<Value, ParseError> {
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

        Ok(Value::String(buf))
    }

    pub fn parse_char(&mut self) -> Result<Value, ParseError> {
        self.consume("\'")?;
        let c = self.current.next().ok_or(ParseError::UnexpectedEnd)?;
        self.consume("\'")?;
        Ok(Value::Char(c))
    }

    pub fn parse_number(&mut self) -> Result<Value, ParseError> {
        let mut number = String::new();
        let mut decimal = false;

        while let Some(&c) = self.current.peek() {
            match c {
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

    pub fn r#match(&mut self, to_match: &'static str) -> Result<bool, ParseError> {
        let mut copy = self.current.clone();

        for c in to_match.chars() {
            match copy.next() {
                Some(c2) if c == c2 => (),
                _ => return Ok(false),
            }
        }

        Ok(true)
    }

    pub fn skip_whitespace(&mut self) -> () {
        while let Some(&c) = self.current.peek() {
            if c.is_whitespace() {
                self.current.next();
            } else {
                break;
            }
        }

        ()
    }
}
