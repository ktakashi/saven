(import (rnrs)
	(bar)
	(srfi :64))

(test-begin "Bar 1")

(test-equal 'bar (bar))

(test-end)
