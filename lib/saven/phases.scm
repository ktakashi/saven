;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven phases)
    (export saven-phase:pre-build
	    saven-phase:build
	    saven-phase:post-build
	    saven-phase:pre-test
	    saven-phase:test
	    saven-phase:post-test
	    saven-phase:package

	    saven:phase-context?
	    )
    (import (rnrs)
	    (saven descriptors))

(define-record-type saven:phase-context
  (fields load-paths
	  test-load-paths
	  working-directory
	  test-working-directory
	  module))

;;; Phases
;; build
;; TODO check 'build' and 'plugin'
;; NB: dependencies will be downloaded into '.sav/dep' directory
(define (saven-phase:pre-build ctx)
  ;; TODO execute pre-build phase tasks

  )
(define (saven-phase:build ctx)
  ;; TODO build it wiht the pre-build result
  )
(define (saven-phase:post-build ctx)
  ;; future hook
  )

;; test
(define (saven-phase:pre-test ctx)
  ;; TODO collect dependencies and compute load path
  ;; NB: dependencies will be downloaded into '.sav/dep' directory
  )
(define (saven-phase:test ctx)
  ;; TODO build it wiht the pre-build result
  )
(define (saven-phase:post-test ctx)
  ;; future hook
  )

;; package
(define (saven-phase:package module ctx)
  ;; TODO collect source into one directory if necessary
  )


)
    
