all: lisp maryl

lisp:
	make -C lisp

maryl:
	make -C maryl

fclean:
	make fclean -C lisp
	make fclean -C maryl

.PHONY: all lisp maryl fclean
