Saugvinon
=========

![](https://github.com/nankeen/sauvignon/workflows/CI/badge.svg)

Saugvinon is a tree walking interpreter written in Rust.

## Building

Rust 2018 is required to compile this project.
Currently, there are no additional dependencies.

```shell
$ cargo build
```

## Quick start syntax

Start the REPL using

```shell
$ cargo run
```

or

```shell
$ cargo build
$ ./target/debug/sauvignon
```

### Data Types

Supported data types

- Integer
- Boolean

### Assignment

Objects can be assigned to identifiers using `let <ident> = <object>`.
```
let thirty = 30;
```

### Functions

Declare functions using the `fn` keyword.

```
let add = fn (a, b) {
   return a + b
}
```

After declaring, you can call the functions.

```
add(5, 2)
```

## TODO:

- [x] Basic Lexer
- [x] Extended Lexer
- [x] REPL
- [x] AST constructor
- [x] Parser
- [x] Internal object system
- [x] Evaluator
