;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins test)
    (export initialize-plugin
	    terminate-plugin)
    (import (rnrs)
	    (saven plugins context)
	    (saven descriptors))

(define-record-type test-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Test plugin"))))))

(define (initialize-plugin config)
  (let ((c (make-test-plugin-context)))
    (saven:plugin-context-register-phase!
     c 'test
     (lambda (phase-ctx) (display "Test plugin") (newline)))))


(define (terminate-plugin context)
  (assert (test-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
