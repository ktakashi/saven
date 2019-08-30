;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins package)
    (export initialize-plugin
	    terminate-plugin)
    (import (rnrs)
	    (saven plugins context)
	    (saven descriptors))

(define-record-type package-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Package plugin"))))))

(define (initialize-plugin config)
  (let ((c (make-package-plugin-context)))
    (saven:plugin-context-register-phase!
     c 'package
     (lambda (phase-ctx) (display "packaging") (newline)))))


(define (terminate-plugin context)
  (assert (package-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
