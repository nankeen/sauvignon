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
- String

### Mathematical expressions

Do integer arithmetic using `+`, `-`, `*`, and `/`.

```
(3+2) * 3
(6 - 1) / 5
20 / 4 - 5
```

### Variable binding

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

### Conditionals

Conditional control flow can be achieved using `if` statements.

```
let a = 5;
if ( a != 5 ) {
  return false;
} else {
  return true;
}
```

### String

String concatenation works using the `+` infix operator.

```
"I like " + "CHEESECAKE!"
```

## TODO:

- [x] Basic Lexer
- [x] Extended Lexer
- [x] REPL
- [x] AST constructor
- [x] Parser
- [x] Internal object system
- [x] Evaluator
- [ ] Built-in functions
- [ ] Arrays
- [ ] Hash maps
