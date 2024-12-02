# glados

Generic Language and Data Operand Syntax

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

----

### Pre-Commit

Pre-commit hooks ensure that your code adheres to consistent formatting, linting, and other quality checks before committing.

#### Install Pre-commit:

```sh
pip install pre-commit
```

#### Install Pre-commit Hooks:

Run the following command to install the pre-commit hooks defined in the .pre-commit-config.yaml file.

```sh
pre-commit install
```

This will set up the hooks for your local repository.


#### Manually Run Pre-commit Hooks:

**You do not need to manually run the hooks after installation, as they will run automatically when you make commits.**

If you'd like to manually run the pre-commit hooks on all files, you can do so with:

```sh
pre-commit run --all-files
```
(Optional)

#### Update Pre-commit Hooks:

To update the pre-commit hooks (in case new versions or hooks are added), run:

```sh
pre-commit autoupdate
```
(Optional)

#### Development Dependencies
Make sure you have the following tools installed:
- stack (Haskell build tool), for its dependencies simply run `stack install`
- pre-commit (Python package)
- pip (for managing Python packages)

## Features

For a detailed list of features, please refer to the [FUNCTIONNALITY.md](FUNCTIONNALITY.md) file.
