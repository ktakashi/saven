;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins build)
    (export saven:initialize-plugin
	    saven:terminate-plugin)
    (import (rnrs)
	    (util file)
	    (saven plugins context)
	    (saven phases)
	    (saven descriptors))

(define-record-type build-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Build plugin"))))))

(define (saven:initialize-plugin config)
  (let ((c (make-build-plugin-context)))
    (saven:plugin-context-register-phase! c 'build
     (execute-building c config))))

(define (execute-building c onfig)
  (lambda (phase-ctx)
    (define working-directory
      (saven:phase-context-working-directory phase-ctx))
    (define source-directories
      (saven:phase-context-source-directories phase-ctx))
    (create-directory* working-directory)
    (for-each (lambda (source-directory)
		(copy-directory source-directory working-directory))
	      source-directories)))

(define (saven:terminate-plugin context)
  (assert (build-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
