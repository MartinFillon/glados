# GLaDOS - Lisp-like Language Interpreter in Haskell

**GLaDOS** project, a minimalist Lisp interpreter implemented in Haskell


## 📜 Features

### 1. Tokenization
- Transforms the input into a stream of **tokens**:
  - **Atoms**:
    - Signed integers in base 10
    - Symbols (any string that is not a number)
  - **Lists**:
    - Parentheses (`(` and `)`)
    - Whitespace-separated expressions

Example:
Input:
```lisp
(define foo 42)
(Symbol "define", Symbol "foo", Integer 42)
```

### 2. Error Handling
- Standardized error messages are displayed to `stderr`.
- The program exits with code `84` on errors, or `0` when successful.
- Errors handled include:
  - **Unbound variables**.
  - **Syntax errors**.
  - **Runtime issues**, such as division by zero.
```lisp
cat error.scm
(* foo 2)
./glados < error.scm
*** ERROR : variable foo is not bound.
echo $?
84
```
```lisp
(define foo)
*** ERROR : invalid syntax in (define foo).
```



### 3. Parsing

- Converts tokens into an **Abstract Syntax Tree (AST)**.
- Supports:
    - **Atoms**: Integers and symbols
    - **Nested lists** for expressions

Example:
Input:
```lisp
(define foo 42)
```
Parsed AST:
```lisp
List [Symbol "define", Symbol "foo", Integer 42]
```

### 4. Expression Evaluation
- Interprets the AST to compute results:
    - **Variable Bindings**:
        - **(define foo 42)** binds **foo** to **42**
        - Throws an error for unbound variables
    - **Function Calls**:
        - Supports arithmetic (**+**, **-**, **X**, **div**, **mod**)
        - Handles user-defined and built-in functions
Example:
Input:
```lisp
(define foo 42)
(+ foo 8)
```
Output:
```lisp
50
```

### 5. User-Defined Functions
- **Lambdas** (anonymous functions):
```lisp
(lambda (x) (* x 2))
```
Creates a function that doubles its input
- **Named Functions**:
```lisp
(define (double x) (* x 2))
```
Defines a function **double** that doubles its input


### 6. Data Types
- Supported types:
    - **64-bit Integers**
    - **Booleans** (**#t** for true, **#f** for false)
    - **Procedures** (lambdas or functions)
- Optional: Add support for **lists** as a type


### 7. Built-In Functions
- Predefined functions:
    - **Predicates:**
        - **(eq? a b)** checks if **a** equals **b**
        - **(< a b)** checks if **a** is less than **b**
    - **Arithmetic:**
        - **+**, **-**, **x**, **div** (integer division), **mod** (modulo)

Example:
```lisp
(+ (* 2 3) (div 10 2)) ; Output: 11
(eq? (* 2 5) (- 11 1)) ; Output: #t
```

### 8. Conditionals
- Conditional expressions with **if**:
    ```lisp
    (if condition then else)
    ```
    Evaluates **then** if **condition** is true, otherwise evaluates **else**

Example:
```lisp
(if (< foo 10) (* foo 3) (div foo 2))
```
Evaluates one of the branches depending on **foo**

### 9. REPL (Read-Eval-Print Loop)
- Interactive mode to:
    - Read Lisp expressions from the terminal.
    - Evaluate them on the fly.
    - Print the results or errors.

### 10. File Input
- Read and execute code from files passed as arguments:
```lisp
./glados script.scm
```

### 11. Testing & CI/CD
- **Unit Tests**:
    - Covers tokenization, parsing, evaluation, and built-in functions.
- **Integration Tests**:
    - Validates end-to-end functionality of the interpreter.
- **Continuous Integration (CI)**:
    - Automates testing on each commit.
- **Continuous Delivery (CD)**:
    - Builds a production-ready executable automatically.


### Summarises

- Final example:
    ```
    ~/B-FUN-500> cat superior.scm
    (define (> a b)
    (if (eq? a b)
    #f
    (if (< a b)
    #f
    #t)))
    (> 10 -2)
    ~/B-FUN-500> ./glados < superior.scm
    #t
    ~/B-FUN-500> cat factorial.scm
    (define (fact x)
    (if (eq? x 1)
    1
    (* x (fact (- x 1)))))
    (fact 10)
    ~/B-FUN-500> ./glados < factorial.scm
    3628800
    ```
- List of functionnality:
    ```
    - Error handling (standardised messages, output to stderr, return code 84).
    - Lexical analysis (tokenisation of atoms, lists and spaces).
    - Syntax analysis (construction of the AST from the tokens).
    - Expression evaluation (interpretation of the AST).
    - Binding management (definition and use of variables).
    - Support for anonymous functions (lambdas).
    - Support for named functions (definition and calling).
    - Implementation of basic types (64-bit integers and Booleans).
    - Implementation of arithmetic operators (+, -, *, div, mod).
    - Implementation of predicates (eq?, <).
    - Management of conditional expressions (if <condition> <then> <else>).
    - Addition of an interactive mode (REPL).
    - Support for reading code from files.
    - Unit tests for each component (tokenisation, parsing, evaluation).
    - Integration tests to validate overall operation.
    - Test automation with a CI/CD pipeline.
    - Automatic generation of a functional executable via the CD pipeline.
    ```

    **Tokenisation**:
    ```lisp
    (define foo 42)
    [(, Symbol "define", Symbol "foo", Integer 42, )]
    ```
    ```lisp
    (+ 3 (* 2 4))
    [(, Symbol "+", Integer 3, (, Symbol "*", Integer 2, Integer 4, ), )]
    ```
    ```lisp
    (lambda (x) (* x x))
    [(, Symbol "lambda", (, Symbol "x", ), (, Symbol "*", Symbol "x", Symbol "x", ), )]
    ```
    **Parsing**:
    ```lisp
    (define foo 42)
    List [Symbol "define", Symbol "foo", Integer 42]
    ```
    ```lisp
    (+ 3 (* 2 4))
    List [Symbol "+", Integer 3, List [Symbol "*", Integer 2, Integer 4]]
    ```
    ```lisp
    (lambda (x) (* x x))
    List [Symbol "lambda", List [Symbol "x"], List [Symbol "*", Symbol "x", Symbol "x"]]
    ```
    **Evalutation**:
    ```lisp
    (define foo 42)
    foo -- 42
    ```
    ```lisp
    (+ 2 3) -- 5
    ```
    ```lisp
    (+ (* 2 3) (div 10 2))
    11
    ```
    **Function**:
    ```lisp
    (lambda (x) (* x x))
    ```
    ```lisp
    ((lambda (x) (* x x)) 3) -- 9
    ```
    ```lisp
    (define (square x) (* x x))
    (square 4) -- 16
    ```
    ```lisp
    (define (fact x)
    (if (eq? x 1)
        1
        (* x (fact (- x 1)))))
    (fact 5) -- 120
    ```
    **Datatype**:
    ```lisp
    (+ 10 32) -- 42
    ```
    ```lisp
    (if #t 1 0) -- 1
    ```
    **Built in**:
    ```lisp
    (eq? 42 42) -- #t
    ```
    ```lisp
    (< 1 2) -- #t
    ```
    **Condition**:
    ```lisp
    (if #t 1 0) -- 1
    ```
    ```lisp
    (define foo 42)
    (if (< foo 10) (* foo 3) (div foo 2)) -- 21
    ```
    **Interactive mode**:
    ```lisp
    ./glados
    --
    > (define foo 10)
    > (+ foo 5)
    15
    > (if (< foo 20) #t #f)
    #t
    ```
    **File Handling**:
    ```lisp
    (define foo 21)
    (* foo 2)
    ```
    ```lisp
    ./glados script.scm -- 42
    ```

### Bonus
There is a configuration file called `list-colors.conf` containing informations on colors used for Megaparsec parsing errors.</br>
If the file doesn't exist, it is created with default colors:
```conf
warnings="255;0;255" ; Magenta
errors="255;0;0"     ; Red
infos="0;0;255"      ; Blue
```
For accessibility purposes, these colors can be changed directly inside the file using format `R;G;B`, or by using the `--setup-colors` flag, formatted like so:
```bash
$> ./glados --setup-colors "warnings:R;G;B errors:R;G;B infos:R;G;B"
```
The order of the `warnings`, `errors` and `infos` parts doesn't matter, as long as all 3 parts are present.</br>
The program will execute as if the flag wasn't present and update the colors inside the file `list-colors.conf`.</br>
Functions returning or getting a `(Color, Color, Color)`, are expecting `(warnings, errors, infos)` in this order.
