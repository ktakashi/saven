;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven phases)
    (export saven-phase:pre-build
	    saven-phase:build
	    saven-phase:post-build
	    saven-phase:pre-test
	    saven-phase:test
	    saven-phase:post-test
	    saven-phase:package)
    (import (rnrs)
	    (saven descriptors))
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
    
