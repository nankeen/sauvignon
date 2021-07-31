#![allow(clippy::enum_variant_names)]
#![allow(clippy::upper_case_acronyms)]

mod evaluator;
mod lexer;
mod parser;
mod repl;

fn main() {
    println!("Sauvignon REPL");
    repl::start(std::io::stdin(), std::io::stdout());
}
