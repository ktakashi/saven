(import (rnrs)
	(rnrs eval)
	(srfi :64))

(test-begin "Baz test")

(test-assert "(foo) is on loadpath"
	     (eval 'foo-proc (environment '(foo))))

(test-end)
