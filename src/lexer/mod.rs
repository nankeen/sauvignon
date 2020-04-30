mod token;
pub use token::Token;

/// Lexer manages the state and tokenization of input
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    input: std::iter::Peekable<std::str::Chars<'a>>,
    cursor: Option<char>,
}

impl<'a> Lexer<'a> {
    /// New `Lexer` with a given input
    ///
    /// # Arguments
    /// * `input` - String containing the program to be parsed
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            input: input.chars().peekable(),
            cursor: None,
        };

        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.cursor = self.input.next();
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(c) = self.cursor {
            if !Lexer::is_identifier(c) {
                break;
            }
            ident.push(c);
            self.read_char();
        }
        ident
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

    fn read_string(&mut self) -> String {
        let mut literal = String::new();
        self.read_char();
        while let Some(c) = self.cursor {
            if c == '\"' {
                break;
            }
            literal.push(c);
            self.read_char();
        }
        self.read_char();
        literal
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.cursor {
            if c != ' ' && c != '\t' && c != '\n' && c != '\r' {
                break;
            }
            self.read_char();
        }
    }

    fn is_identifier(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn lookup_identifier(ident: &str) -> Token {
        match ident {
            // Match against keywords
            "功能" => Token::Function,
            "少於" => Token::LessThan,
            "大於" => Token::GreaterThan,
            "否則" => Token::Else,
            // If it doesn't match, it's an identifier
            s => Token::Ident(s.to_string()),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let tok = match self.cursor {
            Some('當') => match self.input.peek() {
                // Peeks front for `==`
                Some('同') => {
                    self.read_char();
                    Some(Token::Equal)
                }
                _ => Some(Token::Assign),
            },
            Some('讓') => Some(Token::Let),
            Some('正') => Some(Token::BoolLiteral(true)),
            Some('負') => Some(Token::BoolLiteral(false)),
            Some('如') => Some(Token::If),
            Some('歸') => Some(Token::Return),
            Some('+') => Some(Token::Plus),
            Some('-') => Some(Token::Minus),
            Some('不') => match self.input.peek() {
                Some('同') => {
                    self.read_char();
                    Some(Token::NotEqual)
                }
                _ => Some(Token::Bang),
            },
            Some('/') => Some(Token::Slash),
            Some('*') => Some(Token::Asterisk),
            Some('始') => Some(Token::LBrace),
            Some('終') => Some(Token::RBrace),
            Some('(') => Some(Token::LParen),
            Some(')') => Some(Token::RParen),
            Some('[') => Some(Token::LBracket),
            Some(']') => Some(Token::RBracket),
            Some(',') => Some(Token::Comma),
            Some(';') => Some(Token::Semicolon),
            Some(':') => Some(Token::Colon),
            Some('"') => {
                let literal = self.read_string();
                return Some(Token::StringLiteral(literal));
            }
            Some(c) if Lexer::is_identifier(c) => {
                let ident = self.read_identifier();
                return Some(Lexer::lookup_identifier(&ident));
            }
            Some(c) if c.is_digit(10) => {
                let literal = self.read_number();
                return Some(Token::IntLiteral(literal));
            }
            Some(_) => Some(Token::Illegal),
            None => Some(Token::EOF),
        };

        self.read_char();
        tok
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};

    #[test]
    fn test_next_token() {
        let input = "讓 five 當 5;
        讓 ten 當 10;
        讓 add 當 功能(x, y) 始
            x + y;
        終;

        讓 result 當 add(five, ten);
        不-/*5;
        5 少於 10 大於 5;

        如 (5 少於 10) 始
            歸 正;
        終 否則 始
            歸 負;
        終

        10 當同 10;
        10 不同 9;";

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
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::IntLiteral(5),
            Token::LessThan,
            Token::IntLiteral(10),
            Token::GreaterThan,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::IntLiteral(5),
            Token::LessThan,
            Token::IntLiteral(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::BoolLiteral(true),
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::BoolLiteral(false),
            Token::Semicolon,
            Token::RBrace,
            Token::IntLiteral(10),
            Token::Equal,
            Token::IntLiteral(10),
            Token::Semicolon,
            Token::IntLiteral(10),
            Token::NotEqual,
            Token::IntLiteral(9),
            Token::Semicolon,
            Token::StringLiteral("cheesecake".to_string()),
            Token::LBracket,
            Token::IntLiteral(32),
            Token::Comma,
            Token::IntLiteral(64),
            Token::RBracket,
            Token::Semicolon,
            Token::LBrace,
            Token::StringLiteral("cheese".to_string()),
            Token::Colon,
            Token::StringLiteral("cake".to_string()),
            Token::RBrace,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);
        for test in tests {
            match lexer.next() {
                Some(tok) => assert_eq!(test, tok),
                None => panic!("unexpected lexer error: returned None"),
            };
        }
    }
}
