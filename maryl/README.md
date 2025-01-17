# Maryl (Part 2)

More advanced language based on four axis:

- Language-based security and robustness (skill: **security**)
- Its syntax, grammar and semantics (skill: **parsing**)
- The way and how fast it executes code (skill: **evaluation / compilation**)
- Documentation and accessibility (skill: **documentation**)

## Usage

To compile a maryl file into maryl asm use:

```sh
./glados build <file.mrl> -c <output file>
```

To run maryl asm use:

```sh
./glados run <file.masm> [args]
```

## Build

To build the project, we use Makefile and Stack (Haskell's build tool).

To build the project, run the following command:

```sh
make
```

if you want to use the `stack` build system (with formatting), run;

```sh
stack build
```

This will trigger the build process using `stack`, which will compile all necessary Haskell files and their dependencies based on the `stack.yaml` configuration.

This command ensures that all the modules are correctly compiled and linked.

### Clean Project

If you want to remove all the build artifacts and reset the build environment, you can use:

```sh
make fclean
```

This will delete all the build-generated files, including object files and cached dependencies, giving you a clean slate for the next build.

### Testing

We use `Hspec` for unit testing, and `HPC` (Haskell Program Coverage) for measuring test coverage. You can run unit tests and check coverage using `Stack`.

#### Run Tests

To run the unit tests for the project, use the following command:

```sh
make tests_run
```

if you want to use the `stack` build system (with formatting) run;

```sh
stack test
```

This will execute the test suite defined in the project and show the results in your terminal.

#### Run Tests with Coverage

To run the tests with coverage reporting, use this command:

```sh
make coverage
```

This command runs the tests and generates a coverage report. The coverage information is stored in `.tix` files, which are typically processed later to generate a unified coverage report. (THis one is specified with the `Test Spec`'s imports)

You can access the report's path on the last line given by the command with a `hpc_index.html`

#### Viewing Coverage Report

If you want to see a detailed coverage report at root, ensure you have the necessary tools to view it. Stack will generate the `.tix` files, and you can use tools like `hpc` to inspect coverage data.

For example, to visualize coverage:

```sh
make report
```

This command will generate the `hpc_index.html` files at root with coverage on **all** haskell files.

## Features

For a detailed list of features, please refer to the [FEATURES.md](FEATURES.md) file.

## Syntax Highlighting

In order to have syntax highlighing and colors in a maryl file (.mrl) or maryl-asm file (.masm) you need to manually install the extension.
Todo that you can use a command like this:

```sh
cd $HOME/.vscode/extensions
ln -s maryl-language <path to the repository>/maryl/vs-syntax/maryl-language
```

## Maryl Syntax (BNF notation)

```bnf
<type> ::= "int"
        | "float"
        | "string"
        | "char"
        | "bool"
        | "void"
        | <list-type>
        | <const-type>
        | <struct-type>

<list-type> ::= "[]" <type>
<const-type> ::= "const" <type>
<struct-type> ::= "struct" <identifier> <block>
<expression> ::= <variable>
             | <literal>
             | <function-call>
             | <binary-expr>
             | <prefix-expr>
             | <postfix-expr>
             | <ternary-expr>
             | <grouped-expr>
             | <assignment>
             | <label>

<label> ::= <identifier> ":"
<variable> ::= <identifier>
<identifier> ::= <letter> <alphanumeric>*
             | "_" <alphanumeric>+

<literal> ::= <integer>
          | <double>
          | <bool>
          | <string>
          | <char>
          | <list>

<integer> ::= <digit>+
<double> ::= <digit>* "." <digit>+
<bool> ::= "true" | "false"
<string> ::= '"' <char>* '"'
<char> ::= "'" <char> "'"
<list> ::= "[" <expression> ("," <expression>)* "]"
<struct> ::= "{" <expression> ("," <expression>)* "}"

<function-call> ::= <identifier> "(" <expression-list>? ")"
<expression-list> ::= <expression> ("," <expression>)*

<binary-expr> ::= <expression> <binary-operator> <expression>
<binary-operator> ::= "+" | "-" | "|" | "&" | ">>" | "<<" | "^" | "*" | "**" | "/" | "%" | "==" | "!=" | ">" | ">=" | "<" | "<=" | "and" | "or"

<prefix-expr> ::= <prefix-operator> <expression>
<prefix-operator> ::= "!" | "-" | "++" | "--" | "~"

<postfix-expr> ::= <expression> <postfix-operator>
<postfix-operator> ::= "++" | "--"

<ternary-expr> ::= <expression> "?" <expression> ":" <expression>

<grouped-expr> ::= "(" <expression> ")"
<assignment> ::= <variable> <assign-operator> <expression>
<assign-operator> ::= "=" | "+=" | "-=" | "**=" | "*=" | "/=" | "%=" | "|=" | "&=" | "^=" | ">>=" | "<<="
<statement> ::= <declaration>
            | <expression-statement>
            | <if-statement>
            | <while-statement>
            | <return-statement>
            | <break-statement>
            | <continue-statement>

<declaration> ::= <variable-declaration> | <function-declaration>
<variable-declaration> ::= <type> <identifier> ("=" <expression>)? ";"
<function-declaration> ::= <type> <identifier> "(" <parameter-list>? ")" <block>
<parameter-list> ::= <type> <identifier> ("," <type> <identifier>)*
<function-body> ::= <block>
<block> ::= "{" <statement>* "}"

<expression-statement> ::= <expression> ";"

<if-statement> ::= "if" "(" <expression> ")" <block> <else-if>* <else>?
<else-if> ::= "else if" "(" <expression> ")" <block>
<else> ::= "else" <block>

<while-statement> ::= "while" "(" <expression> ")" <block>

<return-statement> ::= "return" <expression>? ";"

<break-statement> ::= "break" ";"
<continue-statement> ::= "continue" ";"

<import-statement> ::= "import" <string> ";"
```
