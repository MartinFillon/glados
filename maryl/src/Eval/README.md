# Maryl Evaluator

The `Evaluator.hs` module is a core component of the Maryl programming language. It evaluates Abstract Syntax Tree (AST) nodes and provides functionality for processing variables, functions, structures, loops, and binary operations. This document provides detailed information about the functionality and usage of the evaluator.

---

## Table of Contents

- [Maryl Evaluator](#maryl-evaluator)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Core Concepts](#core-concepts)
    - [Memory Management](#memory-management)
    - [Supported AST Types](#supported-ast-types)
  - [Functionality](#functionality)
    - [Core Functions](#core-functions)
      - [`evalNode`](#evalnode)
      - [`evalAST`](#evalast)
      - [`applyOp`](#applyop)
    - [Binary Operations](#binary-operations)
    - [Structure Evaluation](#structure-evaluation)
      - [Declaration](#declaration)
      - [Instantiation](#instantiation)
  - [Error Handling](#error-handling)
  - [Usage](#usage)
    - [Example](#example)

---

## Overview

The `Evaluator` processes the Maryl AST by evaluating nodes in memory. It supports:

- Variable and constant declarations.
- Function definitions and calls.
- Loop and conditional execution.
- Binary and unary operations.
- Structure definition and instance creation.

The evaluator works closely with the `Memory` module for managing runtime data.

---

## Core Concepts

### Memory Management

The evaluator uses a `Memory` type to store variables, structures, and functions. Key memory-related operations include:

- **Reading memory**: Fetching variables or functions by name.
- **Updating memory**: Modifying existing variables or adding new definitions.
- **Freeing memory**: Isolating a memory space (e.g., during function calls).

### Supported AST Types

The evaluator processes the following AST types:

- **Variables** (`AstDefineVar`): Define and update variables.
- **Functions** (`AstDefineFunc`, `AstFunc`): Define and call functions.
- **Structures** (`AstDefineStruct`, `AstStruct`): Define and instantiate structures.
- **Binary and Unary Operators** (`AstBinaryFunc`, `AstPrefixFunc`, `AstPostfixFunc`): Perform mathematical and logical operations.
- **Control Flow** (`AstIf`, `AstLoop`): Handle conditionals and loops.

---

## Functionality

### Core Functions

#### `evalNode`
Evaluates a single AST node.

- **Parameters**:
  - `Memory`: The current runtime memory.
  - `Ast`: The AST node to evaluate.
- **Returns**: Either an updated AST with modified memory or an error string.
- **Responsibilities**:
  - Handle variable assignments and updates.
  - Process function calls and loop definitions.
  - Evaluate structure instances and declarations.

#### `evalAST`
Processes a list of AST nodes sequentially.

- **Parameters**:
  - `Memory`: The current runtime memory.
  - `[Ast]`: A list of AST nodes.
- **Returns**: Either an updated list of evaluated ASTs with modified memory or an error string.
- **Use case**: Execute a program block or function body.

#### `applyOp`
Applies a binary operation.

- **Parameters**:
  - `Memory`: The current runtime memory.
  - `String`: The binary operator (e.g., `+`, `-`).
  - `Ast`: The left operand.
  - `Ast`: The right operand.
- **Returns**: Either the result of the operation with updated memory or an error string.
- **Use case**: Arithmetic and logical operations.

### Binary Operations

The evaluator supports a range of binary operations including addition, subtraction, multiplication, division, and logical comparisons. These operations are implemented using a function registry (`defaultRegistry`) for modularity.

### Structure Evaluation

#### Declaration
When defining a structure (`AstDefineStruct`), the evaluator:

1. Validates the field definitions.
2. Stores the structure in memory.

#### Instantiation
When creating an instance of a structure (`AstStruct`), the evaluator:

1. Verifies that the structure exists in memory.
2. Validates field values and types.
3. Returns a normalized structure instance.

---

## Error Handling

The evaluator uses the `Either` type for error handling. Errors include:

- **Type mismatches**: When an AST node's type does not match the expected type.
- **Undefined variables or structures**: When a reference is made to a non-existent entity.
- **Invalid operations**: When an operator is applied to unsupported types.

---

## Usage

To use the evaluator, you need to:

1. **Parse the source code** into an AST using the parser module.
2. **Initialize memory** with predefined structures or variables if needed.
3. **Call `evalAST`** with the AST and memory as input.

### Example

```haskell
import Eval.Evaluator (evalAST)
import Memory (initializeMemory)
import Parsing.ParserAst (parseAST)

main = do
    let sourceCode = "int x = 10; x = x + 5;"
    case parseAST sourceCode of
        Left parseError -> print parseError
        Right ast ->
            case evalAST initializeMemory ast of
                Left evalError -> print evalError
                Right (result, memory) -> do
                    print result
                    print memory
```

---

Feel free to contribute or suggest improvements to this evaluator!

