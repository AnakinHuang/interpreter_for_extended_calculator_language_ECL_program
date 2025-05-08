# 🧮 Interpreter for Extended Calculator Language (ECL)

![License: BSD](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)

## 🚀 Overview

This project implements two complete interpreters for the **Extended Calculator Language (ECL)**, a minimalist, statically scoped, functional programming language. Written in **OCaml**, the interpreters support both basic interpretation and **static analysis + C code generation** for ECL programs.

We developed:
- **Base Interpreter** (`interpreter.ml`): Standard ECL interpreter.
- **Static Analysis Interpreter** (`interpreter_static_analysis.ml`): Adds static semantics checking and equivalent C code generation (extra credit).

---

## 🧩 Language Features

- **Arithmetic:** `+`, `-`, `*`, `/`, `mod`
- **Logical:** `and`, `or`, `not`
- **Comparison:** `=`, `!=`, `<`, `<=`, `>`, `>=`
- **Conditionals:** `if-then-else`
- **Variables:** `let` bindings, scoped properly
- **Recursion:** Full support
- **Static Analysis:** Detects:
    - Use of undeclared variables
    - Redeclaration within the same scope
    - Type mismatches
    - Invalid loop checks

---

## 📂 Project Structure

```
interpreter_for_extended_calculator_language_ECL_program/
├── interpreter.ml                   # Base interpreter
├── interpreter_static_analysis.ml   # Interpreter with static analysis + C code generation
├── Makefile                         # Build commands
├── input/                           # Test programs (.ecl files)
├── README.md
├── README.txt
├── README.pdf
└── (Generated .c files)             # C code output after interpretation
```

---

## ⚙️ Build Instructions

- **Base interpreter:**
  ```bash
  make ecl
  ```

- **Interpreter with static analysis:**
  ```bash
  make ecl_sa
  ```

---

## ▶️ Running the Interpreter

### 1️⃣ Base Interpreter (No Static Analysis)

- Run `prog.ecl` with **stdin** input (Ctrl+D to end):
  ```bash
  make run
  ```

- Run with input file:
  ```bash
  make run_in
  ```

- Run predefined tests:
  ```bash
  make run_test1
  make run_test2
  ...
  make run_test8
  ```

### 2️⃣ Static Analysis + C Code Generation

- Run `prog.ecl` with **stdin** input:
  ```bash
  make run_sa
  ```

- Run with input file:
  ```bash
  make run_sa_in
  ```

- Run predefined tests:
  ```bash
  make run_sa_test1
  make run_sa_test2
  ...
  make run_sa_test8
  ```

💡 **Note:** Input files must be adjusted according to the ECL program being tested.

---

## 🛠️ C Code Generation & Execution

When an ECL program passes interpretation (with or without static analysis), a C source file is **automatically generated** as `[filename].c`.

To compile & run:

```bash
gcc [filename].c -o [executable_name]
./[executable_name]
```

---

## 🧪 Static Analysis Details

Our static analysis enforces **static semantics** by performing a **DFS traversal** over the Abstract Syntax Tree (AST), detecting:
- Undeclared/redeclared variables
- Type mismatches
- Misplaced `check` statements (outside loops)

This ensures your ECL programs are safe **before execution or C code generation**.

---

## ✅ Cleaning the Project

To clean all binary files and generated C source files:

```bash
make clean
```

---

## 💡 Development Notes

- We learned:
    - How to build interpreters in OCaml.
    - How to perform static analysis using AST traversal.
    - How to generate equivalent C code from high-level ECL programs.
- Key insight: OCaml’s strict typing and syntax (e.g., `< >` for inequality instead of `!=`) required careful attention, especially in complex `match` expressions.

---

## 👥 Contributors

- **Yuesong Huang** (yhu116@u.rochester.edu)
- **Wentao Jiang** (wjiang20@u.rochester.edu)

---

## 📄 License

This project is licensed under the BSD 3-Clause License – see the [LICENSE](LICENSE) file for details.

---
