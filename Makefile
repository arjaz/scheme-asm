# TODO: all the machinery to read the program from a file
#       for now you're assumed to write the file from the repl
compile_scheme:
	guile compiler.scm

compile_gcc: compile_scheme
	gcc runtime.c target/scheme.s -o target/scheme.out

run: compile_gcc
	./target/scheme.out
