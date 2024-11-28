##
## EPITECH PROJECT, 2024
## imageCompresor
## File description:
## Makefile
##

BIN_PATH = $(shell stack path --local-install-root)
NAME = glados

TEST_PATH = glados-test.tix

all: $(NAME)

$(NAME):
	$(shell stack build --allow-different-user)
	cp $(BIN_PATH)/bin/$(NAME)-exe $(NAME)

clean:
	stack clean --allow-different-user

fclean: clean
	$(RM) $(NAME)
	$(RM) $(TEST_PATH)
	$(RM) *.html

re: fclean all

tests_run:
	$(shell stack test --allow-different-user)

coverage:
	stack test --coverage

report:
	stack test
	stack exec hpc markup $(TEST_PATH)
	echo "Coverage report generated. Open the \`hpc_index.html\` file in your browser."

.PHONY: all fclean re clean $(NAME) tests_run coverage report
