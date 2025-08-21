use thiserror::Error;
use triomphe::Arc;

use crate::mem::{MemError, ZMem};
use crate::token::Token;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Deref;
use std::pin::Pin;
use std::sync::RwLock;

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
    #[error("Machine error: {0}")]
    Custom(String),
}
impl MachineError {
    pub fn custom(msg: impl Display) -> Self {
        MachineError::Custom(msg.to_string())
    }
}

#[derive(Debug, PartialEq)]
enum Operand {
    Register(String),
    Integer(i64),
    Memory(usize),
}

pub type SharedMachineInner = Arc<RwLock<Machine>>;

#[derive(Clone)]
pub struct SharedMachine(pub SharedMachineInner);

impl Deref for SharedMachine {
    type Target = SharedMachineInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait InterruptHandler {
    type Future: Future<Output = Result<(), MachineError>>;
    fn handle_interrupt(&self, machine: SharedMachineInner) -> Self::Future;
}

pub type BoxedInterruptHandler = Box<
    dyn InterruptHandler<
            Future = Pin<Box<dyn Future<Output = Result<(), MachineError>> + Send + Sync>>,
        > + Send
        + Sync,
>;

pub struct InterruptHandlerErase<H: InterruptHandler>(pub H);
impl<H: InterruptHandler> InterruptHandler for InterruptHandlerErase<H>
where
    H::Future: Send + Sync + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<(), MachineError>> + Send + Sync>>;
    fn handle_interrupt(&self, machine: SharedMachineInner) -> Self::Future {
        Box::pin(self.0.handle_interrupt(machine))
    }
}

pub fn box_interrupt_handler<IH>(handler: IH) -> BoxedInterruptHandler
where
    IH: InterruptHandler + Send + Sync + 'static,
    IH::Future: Send + Sync,
{
    Box::new(InterruptHandlerErase(handler))
}

pub struct InterruptHandlerFn<F>(pub F);
impl<F, Fut> InterruptHandler for InterruptHandlerFn<F>
where
    F: Fn(SharedMachineInner) -> Fut,
    Fut: Future<Output = Result<(), MachineError>>,
{
    type Future = Fut;
    fn handle_interrupt(&self, machine: SharedMachineInner) -> Self::Future {
        (self.0)(machine)
    }
}

pub fn interrupt_handler_fn<IH, Fut>(handler: IH) -> InterruptHandlerFn<IH>
where
    IH: Fn(SharedMachineInner) -> Fut,
    Fut: Future<Output = Result<(), MachineError>>,
{
    InterruptHandlerFn(handler)
}

pub fn boxed_fn<IH, Fut>(handler: IH) -> BoxedInterruptHandler
where
    IH: Fn(SharedMachineInner) -> Fut + Send + Sync + 'static,
    Fut: Future<Output = Result<(), MachineError>> + Send + Sync,
{
    box_interrupt_handler(interrupt_handler_fn(handler))
}

pub struct Machine {
    pub mem: ZMem,
    pub registers: HashMap<String, i64>,
    pc: usize,       // Program Counter for tokens
    data_ptr: usize, // Data Pointer for db
    interrupt_handlers: HashMap<u8, BoxedInterruptHandler>,
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

impl Machine {
    pub fn shared() -> SharedMachine {
        SharedMachine(Arc::new(RwLock::new(Self::new())))
    }
    pub fn new() -> Self {
        Self {
            mem: ZMem::new(),
            registers: HashMap::new(),
            pc: 0,
            data_ptr: 0,
            interrupt_handlers: HashMap::new(),
        }
    }
}
impl SharedMachine {
    pub fn register_interrupt_handler<H>(&self, interrupt_num: u8, handler: H)
    where
        H: InterruptHandler + Send + Sync + 'static,
        H::Future: Send + Sync,
    {
        self.write()
            .unwrap()
            .interrupt_handlers
            .insert(interrupt_num, box_interrupt_handler(handler));
    }

    pub fn register_interrupt_fn<IH, Fut>(&self, interrupt_num: u8, handler: IH)
    where
        IH: Fn(SharedMachineInner) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), MachineError>> + Send + Sync,
    {
        self.write()
            .unwrap()
            .interrupt_handlers
            .insert(interrupt_num, boxed_fn(handler));
    }

    pub async fn run(&self, tokens: &[Token]) -> Result<(), MachineError> {
        self.write().unwrap().pc = 0;
        while self.read().unwrap().pc < tokens.len() {
            let token = &tokens[self.read().unwrap().pc];
            match token {
                Token::Eof => break, // End of program
                Token::Identifier(name) => {
                    self.write().unwrap().pc += 1; // Consume the instruction identifier
                    match name.as_str() {
                        "mov" => self.run_mov(tokens)?,
                        "add" => self.run_add(tokens)?,
                        "int" => self.run_int(tokens).await?,
                        "db" => self.run_db(tokens)?,
                        _ => return Err(MachineError::UnexpectedToken(token.clone())),
                    }
                }
                _ => return Err(MachineError::UnexpectedToken(token.clone())),
            }
        }
        Ok(())
    }

    fn next_token<'b>(&self, tokens: &'b [Token]) -> Result<&'b Token, MachineError> {
        let token = tokens
            .get(self.read().unwrap().pc)
            .ok_or(MachineError::Eof)?;
        self.write().unwrap().pc += 1;
        Ok(token)
    }

    fn get_operand(&self, tokens: &[Token]) -> Result<Operand, MachineError> {
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

    fn expect_comma(&self, tokens: &[Token]) -> Result<(), MachineError> {
        match self.next_token(tokens)? {
            Token::Comma => Ok(()),
            other => Err(MachineError::UnexpectedToken(other.clone())),
        }
    }

    fn run_mov(&self, tokens: &[Token]) -> Result<(), MachineError> {
        let dest = self.get_operand(tokens)?;
        self.expect_comma(tokens)?;
        let src = self.get_operand(tokens)?;

        match (&dest, &src) {
            (Operand::Register(dest_reg), Operand::Register(src_reg)) => {
                let val = *self
                    .read()
                    .unwrap()
                    .registers
                    .get(src_reg.as_str())
                    .unwrap_or(&0);
                self.write()
                    .unwrap()
                    .registers
                    .insert(dest_reg.clone(), val);
            }
            (Operand::Register(dest_reg), Operand::Integer(val)) => {
                self.write()
                    .unwrap()
                    .registers
                    .insert(dest_reg.clone(), *val);
            }
            (Operand::Memory(addr), Operand::Register(src_reg)) => {
                let val = *self
                    .read()
                    .unwrap()
                    .registers
                    .get(src_reg.as_str())
                    .unwrap_or(&0) as u8;
                self.write().unwrap().mem.set(*addr, val)?;
            }
            (Operand::Register(dest_reg), Operand::Memory(addr)) => {
                let val = self.read().unwrap().mem.get(*addr)? as i64;
                self.write()
                    .unwrap()
                    .registers
                    .insert(dest_reg.clone(), val);
            }
            _ => {
                return Err(MachineError::InvalidOperand(Token::Identifier(
                    "Invalid combination of operands for mov".to_string(),
                )));
            }
        }

        Ok(())
    }

    fn run_add(&self, tokens: &[Token]) -> Result<(), MachineError> {
        let dest = self.get_operand(tokens)?;
        self.expect_comma(tokens)?;
        let src = self.get_operand(tokens)?;

        match (&dest, &src) {
            (Operand::Register(dest_reg), Operand::Register(src_reg)) => {
                let val_to_add = *self
                    .read()
                    .unwrap()
                    .registers
                    .get(src_reg.as_str())
                    .unwrap_or(&0);
                *self
                    .write()
                    .unwrap()
                    .registers
                    .entry(dest_reg.clone())
                    .or_insert(0) += val_to_add;
            }
            (Operand::Register(dest_reg), Operand::Integer(val)) => {
                *self
                    .write()
                    .unwrap()
                    .registers
                    .entry(dest_reg.clone())
                    .or_insert(0) += *val;
            }
            _ => {
                return Err(MachineError::InvalidOperand(Token::Identifier(
                    "Invalid combination of operands for add".to_string(),
                )));
            }
        }

        Ok(())
    }

    async fn run_int(&self, tokens: &[Token]) -> Result<(), MachineError> {
        let operand = self.get_operand(tokens)?;
        let interrupt_num = match operand {
            Operand::Integer(val) => val as u8,
            _ => {
                return Err(MachineError::InvalidOperand(Token::Identifier(
                    "Interrupt number must be an integer".to_string(),
                )));
            }
        };

        let handler = self
            .write()
            .unwrap()
            .interrupt_handlers
            .remove(&interrupt_num);
        if let Some(handler) = handler {
            let res = handler.handle_interrupt(self.0.clone()).await;
            self.write()
                .unwrap()
                .interrupt_handlers
                .insert(interrupt_num, handler);
            res
        } else {
            Err(MachineError::UnhandledInterrupt(interrupt_num))
        }
    }

    fn run_db(&self, tokens: &[Token]) -> Result<(), MachineError> {
        loop {
            let token = self.next_token(tokens)?;
            match token {
                Token::Integer(val) => {
                    let data_ptr = self.read().unwrap().data_ptr;
                    self.write().unwrap().mem.set(data_ptr, *val as u8)?;
                    self.write().unwrap().data_ptr += 1;
                }
                Token::String(s) => {
                    for b in s.as_bytes() {
                        let data_ptr = self.read().unwrap().data_ptr;
                        self.write().unwrap().mem.set(data_ptr, *b)?;
                        self.write().unwrap().data_ptr += 1;
                    }
                }
                other => return Err(MachineError::InvalidOperand(other.clone())),
            }

            if self.read().unwrap().pc >= tokens.len() {
                break;
            }

            let next = &tokens[self.read().unwrap().pc];
            if let Token::Comma = next {
                self.write().unwrap().pc += 1; // consume comma
            } else {
                break;
            }
        }
        self.write().unwrap().data_ptr = 0;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        machine::{Machine, interrupt_handler_fn},
        tokenizer::tokenizer,
    };

    #[tokio::test]
    async fn run() {
        let machine = Machine::shared();
        let tokens = tokenizer::tokenize(
            r"
            mov r1, 5
            add r2, r1
            mov [0], r2
            ",
        )
        .unwrap();

        machine.run(&tokens).await.unwrap();
        assert_eq!(machine.read().unwrap().mem[0], 5);
    }

    #[tokio::test]
    async fn interrupt() {
        let machine = Machine::shared();
        machine.register_interrupt_handler(
            0,
            interrupt_handler_fn(async |m| {
                let val = *m.read().unwrap().registers.get("r1").unwrap_or(&0);
                m.write()
                    .unwrap()
                    .registers
                    .insert("r1".to_string(), val + 1);
                Ok(())
            }),
        );

        let tokens = tokenizer::tokenize(
            r"
        mov r1, 5
        int 0
        ",
        )
        .unwrap();

        machine.run(&tokens).await.unwrap();
        assert_eq!(*machine.read().unwrap().registers.get("r1").unwrap(), 6);
    }

    #[tokio::test]
    async fn db() {
        let machine = Machine::shared();
        let tokens = tokenizer::tokenize(
            r#"
            db 10, "hello", 30
            "#,
        )
        .unwrap();

        machine.run(&tokens).await.unwrap();
        let machine = machine.read().unwrap();
        assert_eq!(machine.mem[0], 10);
        assert_eq!(machine.mem[1], b'h');
        assert_eq!(machine.mem[2], b'e');
        assert_eq!(machine.mem[3], b'l');
        assert_eq!(machine.mem[4], b'l');
        assert_eq!(machine.mem[5], b'o');
        assert_eq!(machine.mem[6], 30);
        assert_eq!(machine.data_ptr, 7);
    }

    #[test]
    fn send_sync() {
        let machine = Machine::shared();
        _send_sync(machine.clone());
        _send_sync(machine);
    }
    fn _send_sync<V: Send + Sync + 'static>(_a: V) {
        std::thread::spawn(move || {
            let _a = _a;
        });
    }
}
