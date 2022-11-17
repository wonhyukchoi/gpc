# Gust
This is the repository for the standard, interpreter, compiler, and the standard library for the `Gust` language.

`Gust` is designed to be an safe and fast application-building language with many benefits:
* _Safe_: immutable by default, and garbage collection avoids most segmentation faults.
* _Prototypable_: `Gust` can easily be interpreted, making it easy to prototype.
* _Fast_: When needed, `Gust` can be compiled 
* _Expressive_: Use functional idioms of `map`, `filter`, and `fold`, abstract data types, and higher order functions.
* _Easy_: Garbage collection: you don't need to worry about memory management. Syntax: familiar syntax you've been using all your software career.

## Syntax
GPC syntax is largely influenced by `Rust`.
### Example 1
```
import Vector from vector;
import fold from functional;

int add(int x, int y) {
  x + y;
}

int sum(Vector<int> input) {
  fold(add, 0, input);
} 
```

### Example 2
```
enum Boolean {
  Boolean bool,
  Not Boolean,
  And Boolean Boolean,
  Or Boolean Boolean
}

bool evalBoolean(Boolean boolean) {
  case boolean of {
    Boolean b => b,
    Not b => !evalBoolean(b),
    And b1 b2 => evalBoolean(b1) && evalBoolean(b2),
    Or b1 b2 => evalBoolean(b1) || evalBoolean(b2),
  };
}

```

## Runtime
`GPC` is a garbage-collected language.
This means that there are no pointers in the language; except for primitives, all values in `GPC` follow reference semantics.

This means that `GPC` requires a runtime; the runtime is implemented in `Rust`.

## Compiler
The compiler frontend is written in `Haskell`, which emits `LLVM` code.

## Implementation Milestones
[ ] Sanity Test
    [ ] Basic programs with just primitives: `int`, `bool`, `char`
    [ ] Parser
    [ ] Type Checker
    [ ] Evalualtor
    [ ] `--i` (`--intepreter`) flag
[ ] Debug flags `--debug`, `--ast`, 
[ ] Language Server Protocol
[ ] Compile to LLVM
[ ] Abstract Data Types
[ ] Garbage Collection
[ ] Standard Library
    [ ] String
    [ ] Vector
    [ ] I/O
    [ ] Functional idioms
        [ ] Map
        [ ] Filter
        [ ] Fold
[ ] Bootstrapping
    [ ] Lexer
    [ ] Parser
    [ ] Type Checker
    [ ] Interpreter
