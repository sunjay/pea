# Pea Programming Language

Goal: A general purpose, bytecode-interpreted programming language.

Desired Features:

* High-level
* Functional
* Garbage collected
* Strictly-evaluated
* Expression-oriented
* Pattern matching
* Compiled to bytecode which is then interpreted
* JIT-compiled interpreter
* "Good" error messages
* Eventually: coroutines and async

## Type System

* unit type `()`
* never type `!` - subtypes all types
* arbitrary-precision integers `int`, `uint`
* decimal arithmetic numbers `real`
* structs and enums with fields (including recursive types)
* fixed-sized integers `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `u128`, `i128`
* utf-8 strings `text`
  * utf-8 code point `char`
* dynamically-sized lists `[a]`
    * e.g. `[u8]` is a list of bytes
* fixed-sized arrays `[a; N]` that subtypes `[a]`
* n-tuples `(A, B, ..)` where `A` and `B` are types
* eventually: monomorphized generics and traits
  * `Result` and `Option` types
* eventually: units of measure

## Syntax

* integer literals: `1`, `-1`, `3`, `-1_0__0____0000`
* real literals: `1`, `2.0`, `4.33002`
* ranges: `..`, `1..`, `2..3`, `1..=3`, etc.
* arithmetic operators:
  * `+` = `fn add(self, other: Self) -> Self`
  * `-` = `fn sub(self, other: Self) -> Self`
  * `*` = `fn mul(self, other: Self) -> Self`
  * `/` = `fn div(self, other: Self) -> Self`
  * `%` = `fn rem(self, other: Self) -> Self`
  * `^` = `fn pow(self, other: Self) -> Self`
* boolean operators:
  * `&&` = short-circuiting and
  * `||` = short-circuiting or
  * `!` = `fn not(self) -> Self`
* text literals: `""`, `"abc"`, etc.
  * char literals: `'a'`, `'\u{1234}'`, etc.
* byte list literals: `b""`, `b"abcd"`, etc.
  * byte literals: `b'a'`, etc.
* list literals: `[]`, `[1]`, `[1, 2, 3]`, etc.
  * slicing: `a[1..2]`, etc.
* control flow:
  * `if a { b } else if c { d } else { e }` - expr
  * `while a { b }` - statement
  * `loop { a }` - expr
  * `continue` - expr
  * `break` - expr
* pattern matching: `match a { b }`
  * literals
  * ranges
  * struct fields, enum variants
  * slice patterns
  * or-patterns
  * nested matching
  * pattern guards
* variable declaration: `let pat: type = value;`

## Semantics

* All variables are mutable by default
  * The pattern on the LHS of the declaration must be irrefutable
* All non-primitive values are mutable references which can be passed around
  arbitrarily with no copying (every value is essentially a pointer)
* For now: `panic!()` takes a string and aborts the program with a message
* All lists are copy-on-write, slices only copy if written to
  * No separate type for slices (uses list type)
* All `match` statements are required to be exhaustive

## Name-resolution

To start:

* Names may only be defined at the top-level, order does not matter
* Variables may shadow names
* Types and functions may not shadow other types or functions
* Variables introduced by `use` are available at the top of their current scope
