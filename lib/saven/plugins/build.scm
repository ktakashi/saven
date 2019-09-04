;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins build)
    (export initialize-plugin
	    terminate-plugin)
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

(define (initialize-plugin config)
  (let ((c (make-build-plugin-context)))
    (saven:plugin-context-register-phase! c 'build
     (execute-building c config))))

(define (execute-building c onfig)
  (lambda (phase-ctx)
    (define working-directory
      (saven:phase-context-working-directory phase-ctx))
    (define source-directory
      (saven:phase-context-source-directory phase-ctx))
    (create-directory* working-directory)
    (copy-directory source-directory working-directory)))

(define (terminate-plugin context)
  (assert (build-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
