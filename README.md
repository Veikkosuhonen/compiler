# Hycs [![Rust](https://github.com/Veikkosuhonen/compiler/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/Veikkosuhonen/compiler/actions/workflows/ci.yml)

HY Compilers 2024 project. Can be interpreted as well as compiled to x86_64 assembly.

I also made a [vscode plugin](https://github.com/Veikkosuhonen/hy-compilers-language-support) for it to add syntax highlighting 

## Current features

- All the basic C-like language feature such as variables, control flow, functions
- Handwritten parser
- Interpreter
- Type checker
- Compile to x86_64 assembly
- Pointers
- Heap allocation
- Heap allocated structs
- Lots of unit- and E2E tests, run in Github Actions CI

## Todo

- Arrays
- First-class functions
- Analysis?
