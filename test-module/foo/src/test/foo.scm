(import (rnrs)
	(rnrs eval)
	(foo)
	(srfi :64))

(test-begin "Foo test")

(test-assert "Foo test" #t)
(test-assert "Foo test" (foo-proc #t))

(test-assert "(pffi) is available"
	     (eval 'size-of-char (environment '(pffi))))

(test-end)
