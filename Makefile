log: my.lisp
	buildapp --output $@ --entry log-main --load my.lisp

record: my.lisp
	buildapp --output $@ --entry record-main --load my.lisp

currency: my.lisp
	buildapp --output $@ --entry currency-main --load my.lisp


