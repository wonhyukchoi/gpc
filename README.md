# GPC
This is the repository for the standard, interpreter, and compiler for the GPC language.

## Supported constructs
GPC supports:
* Primitive types: `int`, `float`, `bool`, `char`
* Control flow statements: `if`, `else if`, `else`, `for`, `while`
* Abstract Data Types (pattern matching) and Structs
* Immutable-by-default
* Standard library
    * `IO`, `string`, `vector`, `dict`
    *  Functional `map`, `filter`, `fold` idioms with higher-order functions

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
adt Boolean {
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
