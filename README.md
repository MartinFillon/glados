# glados

Generic Language and Data Operand Syntax

## LISP
You will find every information related to Part 1 of the GLaDOS project in the [`lisp` folder](./lisp/README.md)

## Maryl
You will find every information related to Part 2 of the GLaDOS project in the [`maryl` folder](./maryl/README.md)

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

Additionally, for conventional commits run:
```sh
pre-commit install --hook-type commit-msg
```

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
