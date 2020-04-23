use crate::lexer::{Lexer, Token};
use std::io::{BufRead, Stdin, Stdout, Write};

pub fn start(input: Stdin, mut output: Stdout) {
    print!(">> ");
    output.flush().unwrap();
    for line in input.lock().lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);
        for tok in lexer {
            if tok == Token::EOF {
                break;
            }
            println!("{:?}", tok);
        }
        print!(">> ");
        output.flush().unwrap();
    }
}
