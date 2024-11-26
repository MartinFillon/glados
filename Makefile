##
## EPITECH PROJECT, 2024
## imageCompresor
## File description:
## Makefile
##

BIN_PATH = $(shell stack path --local-install-root)
NAME = glados

all: $(NAME)

$(NAME):
	$(shell stack build --allow-different-user)
	cp $(BIN_PATH)/bin/$(NAME)-exe $(NAME)

clean:
	stack purge --allow-different-user

fclean: clean
	$(RM) $(NAME)

re: fclean all

tests_run:
	$(shell stack test --allow-different-user)

coverage:
	stack test --coverage

report: coverage
	stack exec hpc report --hpcdir .stack-work/dist/x86_64-linux-tinfo6/ghc-9.2.5/hpc

.PHONY: all fclean re clean $(NAME) tests_run coverage report
