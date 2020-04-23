mod lexer;
mod repl;

fn main() {
    println!("Sauvignon REPL");
    repl::start(std::io::stdin(), std::io::stdout());
}
