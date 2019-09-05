(import (rnrs)
	(foo)
	(srfi :64))

(test-begin "Baz test")

(test-assert "(foo) is on loadpath" foo-proc)

(test-end)
