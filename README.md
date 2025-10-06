# GLaDOS: A Lisp-like Interpreter in Haskell

Welcome to GLaDOS, a functional Lisp-like interpreter built from the ground up in Haskell. This project features a custom parsing library, an Abstract Syntax Tree (AST) representation, and an interpreter that supports variables, functions, and core Lisp constructs.

## Features

*   **Interactive REPL**: An interactive Read-Eval-Print Loop that supports multi-line input.
*   **Core Lisp Functionality**:
    *   **Arithmetic**: `+`, `-`, `*`, `div`, `mod`.
    *   **Boolean Logic**: `>`, `<`, `eq?`, and `#t`/`#f` boolean values.
    *   **Control Flow**: `if` expressions for conditional logic.
    *   **Variables**: Global variable definitions using `(define)`.
    *   **First-Class Functions**: Create anonymous functions with `lambda` and define named functions.
    *   **List Manipulation**: `list`, `cons`, `car`, `cdr`, and `null?`.
*   **Custom Parser**: Built using Haskell's `Functor`, `Applicative`, `Alternative`, and `Monad` typeclasses for robust and extensible parsing.
*   **Cross-Platform Builds**: Docker and shell scripts are provided to build and package the interpreter for multiple Linux distributions (Ubuntu, Fedora, Arch).
*   **Comprehensive Test Suite**: Unit tests for parsing, AST generation, and interpreter logic using the Tasty framework.

## Getting Started

### Prerequisites

*   **Stack**: The project is built using the Haskell Tool Stack.
*   **Docker & Docker Compose** (Optional): For building binaries for different Linux distributions.

### Building the Project

You can build the project in several ways:

1.  **Using Makefile**:
    The `Makefile` provides convenient targets for common tasks. To build the executable:
    ```bash
    make build
    ```
    This will compile the project and place the `glados` executable in the project's root directory.

2.  **Using Given Binaries**:
    If you want to build binaries for Ubuntu, Fedora, and Arch Linux, you can use the provided binaries.

### Running the Interpreter

To start the interactive REPL, run the executable:

```bash
make run
```
or directly:
```bash
./glados
```

You will be greeted with a welcome message and a prompt. You can start entering Lisp-like expressions. The REPL will wait for you to balance all parentheses before evaluating the input.

```
Welcome to the GLaDOS Lisp Interpreter!
> (define x 10)
> (+ x 5)
15
> (define (add a b) (+ a b))
> (add 10 20)
30
> 
```
To exit the interpreter, press `Ctrl+D`.

### Running Tests

The project includes a suite of tests to verify its correctness. To run the tests:

```bash
make tests_run
```
This command will execute all tests and generate a code coverage report in the `test/coverage` directory.

## GLaDOS Language Guide

### Data Types

*   **Atom**: An integer value (e.g., `10`, `-5`).
*   **Symbol**: A name used for variables and functions (e.g., `x`, `+`, `my-function`).
*   **Boolean**: `#t` for true and `#f` for false.
*   **List**: A sequence of expressions enclosed in parentheses, which can represent code or data (e.g., `(1 2 3)`, `(+ 1 2)`).

### Core Functions & Special Forms

#### Arithmetic

*   `(+ a b ...)`: Returns the sum of all arguments.
*   `(- a b ...)`: Subtracts the subsequent arguments from the first.
*   `(* a b ...)`: Returns the product of all arguments.
*   `(div a b)`: Returns the integer division of `a` by `b`.
*   `(mod a b)`: Returns the remainder of `a` divided by `b`.

#### Comparisons

*   `(> a b)`: Returns `#t` if `a` is greater than `b`, otherwise `#f`.
*   `(< a b)`: Returns `#t` if `a` is less than `b`, otherwise `#f`.
*   `(eq? a b)`: Returns `#t` if `a` and `b` are equal, otherwise `#f`.

#### List Manipulation

*   `(list a b ...)`: Creates a new list containing the arguments.
*   `(cons element list)`: Adds an `element` to the beginning of a `list`.
*   `(car list)`: Returns the first element of a list.
*   `(cdr list)`: Returns the list without its first element.
*   `(null? list)`: Returns `#t` if the list is empty, otherwise `#f`.

#### `define` (Variables and Functions)

`define` is used to create global bindings.

*   **Variable Binding**:
    ```lisp
    (define my-var 42)
    ```
*   **Function Binding** (shorthand for a lambda):
    ```lisp
    (define (my-func arg1 arg2)
      (+ arg1 arg2))
    ```

#### `if` (Conditionals)

An `if` expression evaluates a condition and executes one of two branches.

*   **Syntax**: `(if <condition> <then-expression> <else-expression>)`
    ```lisp
    (if (> 5 3)
        #t
        #f)
    ```

#### `lambda` (Anonymous Functions)

`lambda` creates an anonymous function that can be called later.

*   **Syntax**: `(lambda (arg1 arg2 ...) <body>)`
    ```lisp
    (define add (lambda (x y) (+ x y)))
    (add 5 3)

    ((lambda (x) (* x x)) 5)
    ```

### Example: N-Queens Solver

The `examples/nqueens.gla` file provides a more complex example of the GLaDOS language, solving the N-Queens problem.

```lisp
(define abs (lambda (n)
  (if (< n 0)
      (- 0 n)
      n)))

(define is-safe? (lambda (new-row board col-dist)
  (if (null? board)
      #t
      (if (eq? new-row (car board))
          #f
          (if (eq? (abs (- new-row (car board))) col-dist)
              #f
              (is-safe? new-row (cdr board) (+ col-dist 1))
          )
      )
  )
))

(define solve (lambda (n col board)
  (if (eq? col n)
      1
      (solve-row-loop n col board 0)
  )
))

(define nqueens (lambda (n)
  (solve n 0 (list))
))

(nqueens 8)
```

## Project Structure

```
.
├── .github/workflows/      # GitHub Actions CI configuration
├── bin/                    # Output directory for compiled binaries
├── docker/                 # Dockerfiles for various Linux distributions
├── examples/               # Example GLaDOS language files
├── src/                    # Main Haskell source code
│   ├── Ast/                # Abstract Syntax Tree definitions and printing
│   │   ├── Ast.hs
│   │   └── AstPrint.hs
│   ├── DataStored.hs       # Core data structures (Ast, Env)
│   ├── Interpret.hs        # The interpreter logic
│   ├── Main.hs             # Main executable entrypoint and REPL
│   ├── ParseSExpr.hs       # S-expression parser
│   └── Tools.hs            # Utility functions
├── test/                   # Test suite
│   ├── Test.hs             # Main test runner
│   ├── TestAst.hs          # Tests for AST conversion
│   ├── TestInterpret.hs    # Tests for the interpreter
│   └── TestParsing.hs      # Tests for the parsing library
├── Parsing/
│   ├── Parsing.hs          # The core parsing library implementation
│   └── ParsingDocumentation.md # Detailed documentation for the parser
├── .gitignore
├── docker-compose.yml      # Docker Compose configuration for builds
├── glados.cabal            # Cabal file (generated by Stack)
├── launch_bin_build        # Script to build and extract binaries via Docker
├── Makefile                # Makefile with helper scripts
├── package.yaml            # Main project definition for Stack
└── stack.yaml              # Stack configuration
```

## Core Parsing Module

The foundation of the interpreter is a custom parsing library located in `Parsing/Parsing.hs`. This module defines a `Parser` type and implements instances for `Functor`, `Applicative`, `Alternative`, and `Monad`, allowing for the creation of complex parsers by combining smaller, simpler ones.

For detailed information on how to use this module, please refer to `Parsing/ParsingDocumentation.md`.
