# GPC
This is the repository for the standard, interpreter, and compiler for the GPC language.

## Supported constructs
GPC supports:
* Primitive types: `int`, `float`, `bool`, `char`
* Control flow statements: `if`, `else if`, `else`, `for`, `while`
* Abstract Data Types (pattern matching) and Structs
* Immutable-by-default
* Standard library: `IO`, `string`, `vector`, `dict`, which follow the functional `map`, `filter`, `fold` idioms

## Runtime
`GPC` is a garbage-collected language.
This means that there are no pointers in the language; except for primitives, all values in `GPC` follow reference semantics.

This means that `GPC` requires a runtime; the runtime is implemented in `Rust`.

## Compiler
The compiler frontend is written in `Haskell`, which emits `LLVM` code.
