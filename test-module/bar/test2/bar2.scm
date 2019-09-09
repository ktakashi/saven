(import (rnrs)
	(bar)
	(srfi :64))

(test-begin "Bar 2")

(test-equal 'bar (bar))

(test-end)
