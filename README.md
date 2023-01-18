# Gust
Welcome to the `Gust` language!

The `Gust` language is designed with a simple core philsophy: ergonomics. Computing was first focused on space complexity, and then on time complexity. But now with the advent of fast and big hardware, we can optimize for _developer productivity_. Not all -- in fact, many -- applications do not need to be performance sensitive. But our time is worth a lot, and `Gust` tries to make sure our time is spent on the ideas instead of the nitty-gritty details. 

The philsophy of `Gust` manifests itself in the following forms:
* Automatic memory management (garbage collection). No need to think about whether this pointer be shared or calculate the variable's lifetime.
* Fast development loop: `Gust` was designed to be easily interpreted so that developers don't need to wait for compilation. But when you need the speed, it can be compiled.
* Functional idioms: `Gust` relies heavily on the functional idioms such as `map`, `filter`, `fold`, etc. 
* Immutable by default: Make bugs hard to write. Changing state is an ever-present source of bugs, so avoid the headache.


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
- [ ] Sanity Test: basic programs with just primitives
    - [x] Parser
    - [ ] Type Checker
    - [ ] `--i` (`--intepreter`) flag
- [ ] Conditionals
    - [ ] `if`
    - [ ] `else`
    - [ ] `elif`
- [ ] Debug flags `--debug`, `--ast`
- [ ] Compile to LLVM
- [ ] Traits
- [ ] Abstract Data Types
- [ ] Garbage Collection
- [ ] Standard Library
    - [ ] String
    - [ ] Vector
    - [ ] I/O
    - [ ] Functional idioms
        - [ ] Map
        - [ ] Filter
        - [ ] Fold
- [ ] Developer Tools
    - [ ] Syntax Highlighting
    - [ ] Language Server Protocol
- [ ] Bootstrapping
    - [ ] Lexer
    - [ ] Parser
    - [ ] Type Checker
    - [ ] Interpreter
