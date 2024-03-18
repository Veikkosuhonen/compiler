# Hycs [![Rust](https://github.com/Veikkosuhonen/compiler/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/Veikkosuhonen/compiler/actions/workflows/ci.yml)

HY Compilers 2024 project. A simple toy-language that can be interpreted as well as compiled to x86_64 assembly.

I also made a [vscode plugin](https://github.com/Veikkosuhonen/hy-compilers-language-support) for it to add syntax highlighting 

## Usage

**Requirements:**
- latest stable version of cargo
- gcc
- x86_64 machine or a suitable emulator/vm

**Optional:**
- graphviz (`dot`)

**Build dev build:**

`cargo build`

output is `./target/debug/compiler`

**Run unit tests:**

`cargo test`


**Commands:**

- `help`: show available commands
- `p <path-to-source>`: Parse the source and output the module definition (output is only for debugging).
- `i <path-to-source>`: Interpret the source.
- `t <path-to-source>`: Typecheck the source and output the module definition (output is only for debugging).
- `ir <path-to-source>`: Output IR for the source (output is only for debugging).
- `asm <path-to-source>`: Output assembly code for the source. Can be then compiled using gcc.
- `dot <path-to-source>`: Output a DOT graph source file for the source based on the IR. Can be visualized with `dot`
- `rd <path-to-source>`: Perform reaching definitions analysis on the IR and print it.
- `lv <path-to-source>`: Perform live variable analysis on the IR and print it.
- `e2e`: Run end-to-end tests, defined in `./test_programs/e2e`
  - `-c`: only run native, no interpreter (useful for benchmarks)
  - `-b`: run benchmarks defined in `./test_programs/benchmarks`. Don't run with the interpreter, its too slow.

**Compiler errors**

The compiler reports tokenization or parsing errors in a readable format. Type-errors and others result in a panic (I didn't have time to implement nice error messages for those).

**Compiler warnings**

The compiler detects and warns about unused writes and unused function returns.

**Compiling code:**

- Compile to assembly using `asm`, example: `./target/debug/compiler asm programs/test.hycs > asm.s`
- Link to binary using gcc: `gcc -g -no-pie -o out asm.s`
- Run: `./out`

**Development helper script:**

```bash
# Runs cargo run -- asm <path>, gcc's and runs the output.
$ ./c.sh <path>
```

## Current features

- Handwritten parser
- Interpreter
- Type checker
- Compile to x86_64 assembly

- All the basic C-like language feature such as...
  - Int and Bool values, local variables
  - Binary and unary operators
  - Builtin functions `read_int`, `print_int`, `print_bool`
  - Block expressions, scopes and shadowing
  - Control flow: if-else and while with break and continue statements

- Functions with return statements
- First-class functions
- Pointers
- Heap allocation
- Heap allocated structs
- Warn about unused writes
- Lots of unit- and E2E tests, run in Github Actions CI
  - Every feature is *quite* thoroughly tested, except analysis and error reporting

## Todo

- Arrays

## Miscellanous notes on implementation

### Tokenizer

To easily support a top level block, the tokenizer wraps the input in curly braces, producing tokens for a single block expression
```
{ <source tokens> }
```
or a `TokenizationError`

### Parser

The parser begins by parsing the top level block, which is otherwise similar to any other block, except it is allowed to contain function and struct definitions. The result of parsing it are the functions, structs and a top level block. The top level block is then composed into 
```kt
fun main(): Unknown <top level block> // More about the Unknown type later
```
The parser finally returns a `Module { functions, structs }` or a `SyntaxError` if something failed during parsing.

### Types and type checker

In addition to the basic types Int, Bool and Unit, there are some more complex types:

- _Pointer\<T>_ where T is any type.
  - A pointer type annotation is a unary expression T*
- _Function_ types, consist of an ordered list of (field_name, Type) pairs, a return type and an optional name.
- _Unknown_: a special type which accepts any other type.
- _Struct_: consists of a name and an ordered list of (field_name, Type) pairs. The ordering defines the memory layout.
  - There is no full type annotation for struct types, they must be referred to by name.
- _Generic_: an internal generic type for generic operators, such as the AddressOf-operator (T) -> Pointer\<T>.
- _Constructor\<T>_: an internal type that represents the result of a call on a _Typeref\<T>_.
  - The result is stack-allocated just like _T_, but can be only used by the New-operator (Constructor\<T>) -> Pointer\<T>.
- _Typeref\<T>_: an internal type that refers to another type _T_.
  - An expression such as `Int`, `Bool` or `Point` - where Point refers to a struct type - have the _Typeref_ type. It is relevant for type annotations. In addition, an expression with a type _Typeref\<T>_ can be called with a signature that returns a _Constructor\<T>_.

### Interpreter

A tree walk interpreter. It is complicated quite a bit by emulating a stack and a heap memory. This made it possible to easily implement pointers and heap allocation interpreting, which otherwise would have required some Rust shared mutable reference trickery which I am not competent enough to do (this is my first Rust project)! The resulting interpreter has the exact same semantics as the compiled language. 

It is very slow, but I haven't benchmarked it against the implementation that had no memory emulation so it's difficult to say how that has affected performance. There is certainly a horrible amount of copying, as I've always taken the easiest path to please the Rust borrow checker. But the only goal of the interpreter is to match the compiled version's semantics.

### IR and the assembly generator

The IR has some special features extending the basic IR of the course. I've added the following instructions:

- `FunctionLabel(name, [arg1, arg2,...])`
- `Declare(var)`

IR variables are also more complex: they have a name, a type and an optional parent. 

IR variables are quite different from normal variables, instead they should be seen as references to certain memory locations that the assembly generator can understand.

## Bugs

Call expression parsing is incorrect, only `<identifier>(<arg list>)` is parsed as a call expression. Expressions such as `get_function()()` are not parsed correctly.

When reporting useless writes, an unused function return is reported as a useless write, which is not accurate. But I'll anyways call it a feature.

The return value of the top level block is not written to the program output. Its not a bug per se, I simply forgot to implement that.
