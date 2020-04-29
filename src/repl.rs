use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{BufRead, Stdin, Stdout, Write};

pub fn start(input: Stdin, mut output: Stdout) {
    print!(">> ");
    output.flush().unwrap();
    let mut evaluator = Evaluator::new();
    for line in input.lock().lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        match program {
            Ok(p) => println!("{:?}", evaluator.eval_program(&p).unwrap()),
            Err(e) => println!("Parser error: {}", e),
        }
        print!(">> ");
        output.flush().unwrap();
    }
}
