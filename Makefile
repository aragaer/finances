DEPS = $(wildcard *.lisp)
all: finances

finances: $(DEPS)
	buildapp --output $@ --entry main --load my.lisp
