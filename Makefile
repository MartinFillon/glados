all: lisp maryl

lisp:
	make -C lisp

maryl:
	make -C maryl

clean:
	make clean -C lisp
	make clean -C maryl

fclean:
	make fclean -C lisp
	make fclean -C maryl

re: 
	make re -C lisp
	make re -C maryl

.PHONY: all lisp maryl clean fclean re
