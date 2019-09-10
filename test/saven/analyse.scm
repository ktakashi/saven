#!/bin/bash
#| -*- mode: scheme; coding: utf-8; -*-
me=$(pwd)/$0
lib=$(dirname $me)/../../lib
cd $(dirname $me)/../../
exec sagittarius -L$lib $me "$@"
|#
(import (rnrs)
	(rnrs r5rs)
	(saven analyse)
	(saven descriptors)
	(srfi :64))

(test-begin "Module descriptor analyser")

(define parent (delay root-module))

(define child0
  (make-saven:module-descriptor
   "child0"
   "1.0"
   '()
   '()
   #f
   "./child0"
   '("src/main")
   '("src/test")
   #f
   parent
   parent))
(define child1
  (make-saven:module-descriptor
   "child1"
   "1.0"
   '()
   '()
   #f
   "./child1"
   '("src/main")
   '("src/test")
   #f
   parent
   parent))
(define child2
  (make-saven:module-descriptor
   "child2"
   "1.0"
   '((module (name "child0"))
     (module (name "child1")))
   '()
   #f
   "./child2"
   '("src/main")
   '("src/test")
   #f
   parent
   parent))

(define root-module
  (make-saven:module-descriptor
   "root"
   "1.0"
   '()
   (list child2 child0 child1)
   #f
   "."
   '("src/main")
   '("src/test")
   #f
   parent
   parent))

(test-equal '("root" "child0" "child1" "child2")
	    (map saven:module-descriptor-name
		 (saven:analyse-descriptor
		  root-module)))
(test-equal root-module (saven:module-descriptor-parent-module child0))
(test-end)

(exit (test-runner-fail-count (test-runner-get)))
