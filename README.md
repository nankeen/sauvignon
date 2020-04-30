Sauvignon
=========

![CI lint & build](https://github.com/nankeen/sauvignon/workflows/CI/badge.svg)
![GitHub last commit](https://img.shields.io/github/last-commit/nankeen/sauvignon)
![GitHub](https://img.shields.io/github/license/nankeen/sauvignon)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/nankeen/sauvignon)

![ForTheBadge built-by-developers](http://ForTheBadge.com/images/badges/built-by-developers.svg)
![ForTheBadge built-with-science](http://ForTheBadge.com/images/badges/built-with-science.svg)
![ForTheBadge powered-by-electricity](http://ForTheBadge.com/images/badges/powered-by-electricity.svg)
![ForTheBadge made-with-crayons](https://forthebadge.com/images/badges/made-with-crayons.svg)
![ForTheBadge uses-badges](http://ForTheBadge.com/images/badges/uses-badges.svg)

![ForTheBadge powered-by-electricity](http://ForTheBadge.com/images/badges/powered-by-electricity.svg)
![ForTheBadge made-with-crayons](https://forthebadge.com/images/badges/made-with-crayons.svg)

Sauvignon is a tree walking interpreter written in Rust.

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

Objects can be assigned to identifiers using `讓 <ident> 當 <expression>`.

```
讓 三十 當 30;
```

### Functions

Declare functions using the `功能` keyword.

```
讓 加 當 功能(a, b) 始
   歸 a + b
終
```

After declaring, you can call the functions.

```
加(5, 2)
```

### Conditionals

Conditional control flow can be achieved using `如`...`否則` statements.

```
讓 a 當 5;
如 ( a 不是 5 ) 始
  歸 負;
終 否則 始
  歸 正;
終
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
- [x] Built-in functions
- [x] Arrays
- [x] Hash maps
- [ ] Module import
