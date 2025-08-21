use zkt_asm::{machine::Machine, tokenizer::tokenizer};

#[tokio::main]
async fn main() {
    let machine = Machine::shared();

    // Register interrupt 1 to print a null-terminated string from memory
    machine.register_interrupt_fn(1, async |m| {
        let addr = *m.read().unwrap().registers.get("r1").unwrap_or(&0) as usize;
        let mut bytes = Vec::new();
        let mut current_addr = addr;
        loop {
            let byte = m.read().unwrap().mem.get(current_addr)?;
            if byte == 0 {
                break;
            }
            bytes.push(byte);
            current_addr += 1;
        }
        let s = String::from_utf8(bytes).unwrap();
        println!("{s}");
        Ok(())
    });

    // Assembly code to store a string and trigger the interrupt
    let code = r#"
        db "Hello, World!", 0
        mov r1, 0
        int 1
    "#;

    // Tokenize and run the code
    let tokens = tokenizer::tokenize(code).unwrap();
    machine.run(&tokens).await.unwrap();
}
