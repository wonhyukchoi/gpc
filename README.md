# Garbage-Collected Pico-C 
This is the repository for the standard and compiler for Garbage-Collected Pico-C (`GPC`).

## Why "Pico"?
The "Pico" in `GPC` is because `GPC` only supports a very limited subset of C constructs.
It does support:
* Primitives: `int`, `bool`, `char`
* Control flows: `if`, `for`, `while`
* `array`s and `struct`s
* `IO`, `string`, `vector`, and `dict` standard libraries

In particular, `GPC` should be able to implement most (if not all) of the common data structures & algorithms problems.

## Runtime
`GPC` is a garbage-collected language.
This means that there are no pointers in the language; except for primitives, all values in `GPC` follow reference semantics.

This means that `GPC` requires a runtime; the runtime is implemented in `Rust`.

## Compiler
The compiler frontend is written in `Haskell`, which emits `LLVM` code.
