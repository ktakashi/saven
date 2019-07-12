;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle)
    (import (rnrs)
	    (saven descriptors))

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

;;; Phases
;; build
;; TODO check 'build' and 'plugin'
(define (saven-phase:pre-build module)
  ;; TODO collect dependencies and compute load path
  ;; NB: dependencies will be downloaded into '.sav/dep/main' directory
  )
(define (saven-phase:build module)
  ;; TODO build it wiht the pre-build result
  )
(define (saven-phase:post-build module)
  ;; future hook
  )

;; test
(define (saven-phase:pre-test module)
  ;; TODO collect dependencies and compute load path
  ;; NB: dependencies will be downloaded into '.sav/dep/test' directory
  )
(define (saven-phase:test module)
  ;; TODO build it wiht the pre-build result
  )
(define (saven-phase:post-test module)
  ;; future hook
  )

;; package
(define (saven-phase:package module)
  ;; TODO collect source into one directory if necessary
  )


)
	    
