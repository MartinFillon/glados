# glados

Generic Language and Data Operand Syntax

### Testing

We use `Hspec` for unit testing, and `HPC` (Haskell Program Coverage) for measuring test coverage. You can run unit tests and check coverage using `Stack`.

#### Run Tests

To run the unit tests for the project, use the following command:

```sh
stack test
```

This will execute the test suite defined in the project and show the results in your terminal.

#### Run Tests with Coverage

To run the tests with coverage reporting, use this command:

```sh
stack test --coverage
```

This command runs the tests and generates a coverage report. The coverage information is stored in `.tix` files, which are typically processed later to generate a unified coverage report.

#### Viewing Coverage Report

If you want to see a detailed coverage report, ensure you have the necessary tools to view it. Stack will generate the `.tix` files, and you can use tools like `hpc` to inspect coverage data.

For example, to visualize coverage:

```sh
stack exec hpc report --hpcdir .stack-work/dist/x86_64-linux-tinfo6/ghc-9.2.5/hpc
```

This command shows which lines of code were executed during the test run, helping you identify untested areas.
