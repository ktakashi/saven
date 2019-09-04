(import (rnrs)
	#;(foo)
	(srfi :64))

(test-begin "Foo test")

(test-assert "Foo test" #t)
;;(test-assert "Foo test" (foo-proc #t))

(test-end)
