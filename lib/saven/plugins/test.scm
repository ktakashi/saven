;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins test)
    (export initialize-plugin
	    terminate-plugin)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius process)
	    (saven phases)
	    (saven plugins context)
	    (saven descriptors)
	    (srfi :13 strings)
	    (util file))

(define-record-type test-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Test plugin"))))))

(define (initialize-plugin config)
  (let ((c (make-test-plugin-context)))
    (saven:plugin-context-register-phase! c 'test
     (test-plugin-executor c config))))

(define +default-process-name+
  (cond-expand (windows "sash") (else "sagittarius")))
(define +default-load-path-prefix+ "-L")
(define +default-argument-terminator+ "--")

(define (test-plugin-executor c config)
  (define (assq/default n l default)
    (cond ((assq n l) => cadr) (else default)))
  (define process (assq/default 'process config +default-process-name+))
  (define prefix (assq/default 'path-prefix config +default-load-path-prefix+))
  (define terminator
    (assq/default 'argument-terminator config +default-argument-terminator+))
  (lambda (phase-ctx)
    (define working-directory
      (saven:phase-context-working-directory phase-ctx))
    (define load-paths
      (saven:phase-context-load-paths phase-ctx))
    (define test-load-paths
      (saven:phase-context-test-load-paths phase-ctx))
    (define test-source-directory
      (saven:phase-context-test-source-directory phase-ctx))
    (define test-working-directory
      (saven:phase-context-test-working-directory phase-ctx))
    ;; create test working directory, if it's not there yet
    (create-directory* test-working-directory)
    (copy-directory test-source-directory test-working-directory)
    (let* ((test-files (find-files test-working-directory :pattern "\\.scm$"))
	   (paths (append load-paths test-load-paths))
	   (p&r* (map (lambda (file)
			(run-it process terminator
			 (map (lambda (p) (string-append prefix p)) paths)
			 file)) test-files)))
      ;; (for-each process-wait p*)
      (map extract-result p&r*))))

(define (run-it name terminator load-paths file)
  (let ((p (create-process name `(,@load-paths ,terminator ,file))))
    (cons p (process-wait p))))
    
(define (extract-result p&r)
  (let* ((p (car p&r))
	 (in (process-output-port p))
	 (err (process-error-port p)))
    (let ((inb (get-bytevector-all in))
	  (errb (get-bytevector-all err)))
      (unless (eof-object? errb) (display (utf8->string errb)))
      (unless (eof-object? inb) (display (utf8->string inb))))))

(define (terminate-plugin context)
  (assert (test-plugin-context? context))
  ;; TODO terminate process if needed
  )
)