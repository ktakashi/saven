;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven plugins test)
    (export saven:initialize-plugin
	    saven:terminate-plugin)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius process)
	    (saven phases)
	    (saven console)
	    (saven plugins context)
	    (saven descriptors)
	    (srfi :13 strings)
	    (util file))

(define-record-type test-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Test plugin"))))))

(define (saven:initialize-plugin config)
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
    (define test-source-directories
      (saven:phase-context-test-source-directories phase-ctx))
    (define test-working-directory
      (saven:phase-context-test-working-directory phase-ctx))
    ;; create test working directory, if it's not there yet
    (create-directory* test-working-directory)
    (for-each (lambda (test-source-directory)
		(copy-directory test-source-directory test-working-directory))
	      test-source-directories)
    (let* ((test-files (find-files test-working-directory
				   :pattern "(?:\\.scm$|\\.sps$)"))
	   (paths (append (list working-directory) load-paths test-load-paths))
	   (p&r* (map (lambda (file)
			(run-it process terminator
			 (map (lambda (p) (string-append prefix p)) paths)
			 file)) test-files)))
      ;; (for-each process-wait p*)
      (map extract-result p&r*))))

(define (run-it name terminator load-paths file)
  (let ((p (create-process name `(,@load-paths ,terminator ,file))))
    (cons* p (process-wait p) file)))
    
(define (extract-result p&r)
  (define (get-message errb inb)
    (let-values (((o e) (open-string-output-port)))
      (unless (eof-object? errb) (display (utf8->string errb) o))
      (unless (eof-object? inb) (display (utf8->string inb) o))
      (e)))
  (let* ((p (car p&r))
	 (f (cddr p&r))
	 (in (process-output-port p))
	 (err (process-error-port p)))
    (let ((inb (get-bytevector-all in))
	  (errb (get-bytevector-all err)))
      (saven:console-info-write "Result of '~a'~%~a" f
				(get-message errb inb)))))

(define (saven:terminate-plugin context)
  (assert (test-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
