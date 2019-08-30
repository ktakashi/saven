;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven plugins context)
    (export (rename (saven:plugin-context <saven:plugin-context>))
	    saven:plugin-context?
	    saven:plugin-context-plugin-name
	    saven:plugin-context-register-phase!
	    saven:plugin-context-execute!)
    (import (rnrs))

(define-record-type saven:plugin-context
  (fields plugin-name
	  phases)
  (protocol (lambda (p)
	      (lambda (n)
		(p n (make-eq-hashtable))))))

(define (saven:plugin-context-register-phase! context phase callback)
  (hashtable-update! (saven:plugin-context-phases context) phase
		     (lambda (v) (cons callback v)) '())
  context)

(define (saven:plugin-context-execute! context phase phase-context)
  (cond ((hashtable-ref (saven:plugin-context-phases context) phase #f) =>
	 (lambda (callbacks)
	   (for-each (lambda (c) (c phase-context)) callbacks)))))
)
