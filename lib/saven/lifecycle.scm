;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle)
    (import (rnrs)
	    (sagittarius)
	    (saven descriptors)
	    (saven phases)
	    (saven execution))

(define (saven:lifecycle modules)
  (define executions (map saven:execution modules))
  (lambda (targets)
    (fold-left (lambda (results execution) (cons (execution targets) results))
	       '() executions)))

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
	    
