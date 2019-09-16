;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle
	    saven:push-cleanup!)
    (import (rnrs)
	    (sagittarius)
	    (srfi :39 parameters)
	    (saven console)
	    (saven descriptors)
	    (saven phases)
	    (saven execution))

(define *cleanups* (make-parameter '()))
(define (saven:push-cleanup! cleanup)
  (assert (procedure? cleanup))
  (*cleanups* (cons cleanup (*cleanups*))))

(define (saven:lifecycle modules)
  (saven:console-info-write "Execution order")
  (for-each (lambda (m)
	      (saven:console-info-write " - ~a"
					(saven:module-descriptor-name m)))
	    modules)
  (let ((executions (map saven:execution modules)))
    (lambda (targets)
      (parameterize ((*cleanups* '()))
	(fold-left (lambda (results execution)
		     (cons (execution targets) results))
		   '() executions)
	(for-each (lambda (thunk) (thunk)) (*cleanups*)))
      (saven:console-info-write "Finished"))))

;; phase and builtin target mapping
;; - pre-build   +---+
;;               |   |
;; - build       +---+-- build
;;               |   |
;; - post-build  +---+
;;               |
;; - pre-test    +---+
;;		 |   |
;; - test	 +---+-- test
;;		 |   |
;; - post-test	 +---+
;;               |
;; - package     +------ package
;; 
;;                       clean (clean doesn't have any phase)
;; 
;; users will be able to extend phase behaviour (not now)


)
	    
