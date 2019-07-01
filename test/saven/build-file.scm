#!/bin/bash
#| -*- mode: scheme; coding: utf-8; -*-
me=$(pwd)/$0
lib=$(dirname $me)/../../lib
cd $(dirname $me)/../../
exec sagittarius -L$lib $me "$@"
|#
(import (rnrs)
	(saven build-file)
	(sagittarius)
	(srfi :64))

(define data-directory (build-path (current-directory) "test/data"))

(test-begin "Build file reader")

(define expected
  '(saven
    (dependencies
     (github
      (name "ktakashi/r6rs-pffi")
      (release "master"))
     (github
      (name "ktakashi/r7rs-postgresql")
      (release "master")
      (scope "test")))
    (modules "foo" "bar")))

(define (read-test file)
  (test-equal file expected
	      (saven:read-build-file (build-path data-directory file))))

(read-test "build-files/test.yaml")
(read-test "build-files/test.json")
(read-test "build-files/test.scm")

(test-end)
