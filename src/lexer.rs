#![allow(dead_code)]
#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    IntLiteral(i64),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    input: std::str::Chars<'a>,
    cursor: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            input: input.chars(),
            cursor: None,
        };

        lexer.read_char();
        return lexer;
    }

    fn read_char(&mut self) {
        self.cursor = self.input.next();
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(c) = self.cursor {
            if !Lexer::is_identifier(&c) {
                break;
            }
            ident.push(c);
            self.read_char();
        }
        return ident;
    }

    fn read_number(&mut self) -> i64 {
        let mut number = String::new();
        while let Some(c) = self.cursor {
            if !c.is_digit(10) {
                break;
            }
            number.push(c);
            self.read_char();
        }
        number.parse().unwrap()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.cursor {
            if c != ' ' && c != '\t' && c != '\n' && c != '\r' {
                break;
            }
            self.read_char();
        }
    }

    fn is_identifier(c: &char) -> bool {
        c.is_alphabetic() || *c == '_'
    }

    fn lookup_identifier(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            s => Token::Ident(s.to_string()),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let tok = match self.cursor {
            Some('=') => Some(Token::Assign),
            Some(';') => Some(Token::Semicolon),
            Some('(') => Some(Token::LParen),
            Some(')') => Some(Token::RParen),
            Some(',') => Some(Token::Comma),
            Some('+') => Some(Token::Plus),
            Some('{') => Some(Token::LBrace),
            Some('}') => Some(Token::RBrace),
            Some(c) if Lexer::is_identifier(&c) => {
                let ident = self.read_identifier();
                return Some(Lexer::lookup_identifier(&ident));
            },
            Some(c) if c.is_digit(10) => {
                let literal = self.read_number();
                return Some(Token::IntLiteral(literal));
            },
            Some(_) => Some(Token::Illegal),
            None => Some(Token::EOF),
        };

        self.read_char();
        return tok;
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, Lexer};

    #[test]
    fn test_next_token(){
        let input = "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };";

        let tests = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::IntLiteral(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::EOF
        ];

        let mut lexer = Lexer::new(input);
        for test in &tests {
            match lexer.next() {
                Some(tok) => assert_eq!(*test, tok),
                None => panic!("unexpected lexer error: returned None")
            };
        }
    }
}
