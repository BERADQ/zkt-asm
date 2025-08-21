use thiserror::Error;

use crate::mem::{MemError, ZMem};
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Error)]
pub enum MachineError {
    #[error("Unexpected token {0:?}")]
    UnexpectedToken(Token),
    #[error("Invalid operand {0:?}")]
    InvalidOperand(Token),
    #[error("Missing operand")]
    MissingOperand,
    #[error("End of file")]
    Eof,
    #[error("Unhandled interrupt {0}")]
    UnhandledInterrupt(u8),
    #[error("Memory error: {0}")]
    MemoryError(#[from] MemError),
}

#[derive(Debug, PartialEq)]
enum Operand {
    Register(String),
    Integer(i64),
    Memory(usize),
}

type InterruptHandler<'a> =
    Box<dyn Fn(&mut Machine) -> Result<(), MachineError> + Send + Sync + 'a>;

pub struct Machine<'a> {
    pub mem: ZMem,
    pub registers: HashMap<String, i64>,
    pc: usize,       // Program Counter for tokens
    data_ptr: usize, // Data Pointer for db
    interrupt_handlers: HashMap<u8, InterruptHandler<'a>>,
}

impl Default for Machine<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Machine<'a> {
    pub fn new() -> Self {
        Machine {
            mem: ZMem::new(),
            registers: HashMap::new(),
            pc: 0,
            data_ptr: 0,
            interrupt_handlers: HashMap::new(),
        }
    }

    pub fn register_interrupt_handler<F>(&mut self, interrupt_num: u8, handler: F)
    where
        F: Fn(&mut Machine) -> Result<(), MachineError> + Send + Sync + 'a,
    {
        self.interrupt_handlers
            .insert(interrupt_num, Box::new(handler));
    }

    pub fn run(&mut self, tokens: &[Token]) -> Result<(), MachineError> {
        self.pc = 0;
        while self.pc < tokens.len() {
            let token = &tokens[self.pc];
            match token {
                Token::Eof => break, // End of program
                Token::Identifier(name) => {
                    self.pc += 1; // Consume the instruction identifier
                    match name.as_str() {
                        "mov" => self.run_mov(tokens)?,
                        "add" => self.run_add(tokens)?,
                        "int" => self.run_int(tokens)?,
                        "db" => self.run_db(tokens)?,
                        _ => return Err(MachineError::UnexpectedToken(token.clone())),
                    }
                }
                _ => return Err(MachineError::UnexpectedToken(token.clone())),
            }
        }
        Ok(())
    }

    fn next_token<'b>(&mut self, tokens: &'b [Token]) -> Result<&'b Token, MachineError> {
        let token = tokens.get(self.pc).ok_or(MachineError::Eof)?;
        self.pc += 1;
        Ok(token)
    }

    fn get_operand(&mut self, tokens: &[Token]) -> Result<Operand, MachineError> {
        match self.next_token(tokens)? {
            Token::Register(name) => Ok(Operand::Register(name.clone())),
            Token::Integer(val) => Ok(Operand::Integer(*val)),
            Token::LBracket => {
                let addr = match self.next_token(tokens)? {
                    Token::Integer(val) => *val as usize,
                    other => return Err(MachineError::InvalidOperand(other.clone())),
                };
                match self.next_token(tokens)? {
                    Token::RBracket => Ok(Operand::Memory(addr)),
                    other => Err(MachineError::UnexpectedToken(other.clone())),
                }
            }
            other => Err(MachineError::InvalidOperand(other.clone())),
        }
    }

    fn expect_comma(&mut self, tokens: &[Token]) -> Result<(), MachineError> {
        match self.next_token(tokens)? {
            Token::Comma => Ok(()),
            other => Err(MachineError::UnexpectedToken(other.clone())),
        }
    }

    fn run_mov(&mut self, tokens: &[Token]) -> Result<(), MachineError> {
        let dest = self.get_operand(tokens)?;
        self.expect_comma(tokens)?;
        let src = self.get_operand(tokens)?;

        match (&dest, &src) {
            (Operand::Register(dest_reg), Operand::Register(src_reg)) => {
                let val = *self.registers.get(src_reg.as_str()).unwrap_or(&0);
                self.registers.insert(dest_reg.clone(), val);
            }
            (Operand::Register(dest_reg), Operand::Integer(val)) => {
                self.registers.insert(dest_reg.clone(), *val);
            }
            (Operand::Memory(addr), Operand::Register(src_reg)) => {
                let val = *self.registers.get(src_reg.as_str()).unwrap_or(&0) as u8;
                self.mem.set(*addr, val)?;
            }
            (Operand::Register(dest_reg), Operand::Memory(addr)) => {
                let val = self.mem.get(*addr)? as i64;
                self.registers.insert(dest_reg.clone(), val);
            }
            _ => {
                return Err(MachineError::InvalidOperand(Token::Identifier(
                    "Invalid combination of operands for mov".to_string(),
                )));
            }
        }

        Ok(())
    }

    fn run_add(&mut self, tokens: &[Token]) -> Result<(), MachineError> {
        let dest = self.get_operand(tokens)?;
        self.expect_comma(tokens)?;
        let src = self.get_operand(tokens)?;

        match (&dest, &src) {
            (Operand::Register(dest_reg), Operand::Register(src_reg)) => {
                let val_to_add = *self.registers.get(src_reg.as_str()).unwrap_or(&0);
                let entry = self.registers.entry(dest_reg.clone()).or_insert(0);
                *entry += val_to_add;
            }
            (Operand::Register(dest_reg), Operand::Integer(val)) => {
                let entry = self.registers.entry(dest_reg.clone()).or_insert(0);
                *entry += *val;
            }
            _ => {
                return Err(MachineError::InvalidOperand(Token::Identifier(
                    "Invalid combination of operands for add".to_string(),
                )));
            }
        }

        Ok(())
    }

    fn run_int(&mut self, tokens: &[Token]) -> Result<(), MachineError> {
        let operand = self.get_operand(tokens)?;
        let interrupt_num = match operand {
            Operand::Integer(val) => val as u8,
            _ => {
                return Err(MachineError::InvalidOperand(Token::Identifier(
                    "Interrupt number must be an integer".to_string(),
                )));
            }
        };

        if let Some(handler) = self.interrupt_handlers.remove(&interrupt_num) {
            let res = handler(self);
            self.interrupt_handlers.insert(interrupt_num, handler);
            res
        } else {
            Err(MachineError::UnhandledInterrupt(interrupt_num))
        }
    }

    fn run_db(&mut self, tokens: &[Token]) -> Result<(), MachineError> {
        loop {
            let token = self.next_token(tokens)?;
            match token {
                Token::Integer(val) => {
                    self.mem.set(self.data_ptr, *val as u8)?;
                    self.data_ptr += 1;
                }
                Token::String(s) => {
                    for b in s.as_bytes() {
                        self.mem.set(self.data_ptr, *b)?;
                        self.data_ptr += 1;
                    }
                }
                other => return Err(MachineError::InvalidOperand(other.clone())),
            }

            if self.pc >= tokens.len() {
                break;
            }

            let next = &tokens[self.pc];
            if let Token::Comma = next {
                self.pc += 1; // consume comma
            } else {
                break;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{machine::Machine, tokenizer::tokenizer};

    #[test]
    fn run() {
        let mut machine = Machine::new();
        let tokens = tokenizer::tokenize(
            r"
            mov r1, 5
            add r2, r1
            mov [0], r2
            ",
        )
        .unwrap();

        machine.run(&tokens).unwrap();
        assert_eq!(machine.mem[0], 5);
    }

    #[test]
    fn interrupt() {
        let mut machine = Machine::new();
        machine.register_interrupt_handler(0, |m| {
            let val = m.registers.get("r1").unwrap_or(&0);
            m.registers.insert("r1".to_string(), val + 1);
            Ok(())
        });

        let tokens = tokenizer::tokenize(
            r"
        mov r1, 5
        int 0
        ",
        )
        .unwrap();

        machine.run(&tokens).unwrap();
        assert_eq!(*machine.registers.get("r1").unwrap(), 6);
    }

    #[test]
    fn db() {
        let mut machine = Machine::new();
        let tokens = tokenizer::tokenize(
            r#"
            db 10, "hello", 30
            "#,
        )
        .unwrap();

        machine.run(&tokens).unwrap();
        assert_eq!(machine.mem[0], 10);
        assert_eq!(machine.mem[1], b'h');
        assert_eq!(machine.mem[2], b'e');
        assert_eq!(machine.mem[3], b'l');
        assert_eq!(machine.mem[4], b'l');
        assert_eq!(machine.mem[5], b'o');
        assert_eq!(machine.mem[6], 30);
        assert_eq!(machine.data_ptr, 7);
    }
}
