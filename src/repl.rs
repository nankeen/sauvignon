use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{BufRead, Stdin, Stdout, Write};

pub fn start(input: Stdin, mut output: Stdout) {
    print!(">> ");
    output.flush().unwrap();
    for line in input.lock().lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        match program {
            Ok(p) => println!("{:?}", p),
            Err(e) => println!("Parser error: {}", e),
        }
        print!(">> ");
        output.flush().unwrap();
    }
}
