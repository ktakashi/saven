;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins clean)
    (export initialize-plugin
	    terminate-plugin)
    (import (rnrs)
	    (util file)
	    (saven plugins context)
	    (saven console)
	    (saven phases))

(define-record-type clean-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Clean plugin"))))))

(define (initialize-plugin config)
  (let ((c (make-clean-plugin-context)))
    (saven:plugin-context-register-phase! c 'clean
     (execute-cleaning c config))))

(define (execute-cleaning c onfig)
  (lambda (phase-ctx)
    (define target-directory
      (saven:phase-context-target-directory phase-ctx))
    (saven:console-info-write "Removing ~a" target-directory)
    (when (file-exists? target-directory)
      (delete-directory* target-directory))))

(define (terminate-plugin context)
  (assert (clean-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
