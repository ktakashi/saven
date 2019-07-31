#!/bin/bash
#| -*- mode: scheme; coding: utf-8; -*-
me=$(pwd)/$0
lib=$(dirname $me)/../../lib
cd $(dirname $me)/../../
exec sagittarius -L$lib $me "$@"
|#
(import (rnrs)
	(saven descriptors)
	(util file)
	(srfi :1)
	(srfi :64))

(define root-sav-file (build-path* (current-directory) "test-module" "sav.scm"))

(test-begin "Descriptor")

(test-assert (saven:module-descriptor?
	      (saven:build-file->module-descriptor root-sav-file)))
(let ((descriptor (saven:build-file->module-descriptor root-sav-file)))
  (test-equal "version" "1.0" (saven:module-descriptor-version descriptor))
  (test-equal 3 (length (saven:module-descriptor-modules descriptor)))
  (test-equal '(#t #t #t) (map saven:module-descriptor?
			       (saven:module-descriptor-modules descriptor)))
  (test-assert (lset= string=? '("foo" "bar" "baz")
		      (map saven:module-descriptor-name
			   (saven:module-descriptor-modules descriptor))))
  (test-equal 2 (length (saven:module-descriptor-dependencies descriptor))))

(test-end)
(exit (test-runner-fail-count (test-runner-get)))
