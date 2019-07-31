;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle)
    (import (rnrs)
	    (sagittarius)
	    (saven descriptors)
	    (saven phases))

(define (saven:lifecycle modules)
  (define executions (map saven:module->execution modules))
  (lambda (targets)
    (fold-left (lambda (results execution) (cons (execution targets) results))
	       '() executions)))

(define (saven:module->execution module)
  
  (lambda (targets)
    (display (saven:module-descriptor-name module)) (newline)))

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
	    
