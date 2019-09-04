;;; -*- mode: scheme; coding: utf-8 -*-
(library (saven plugins package)
    (export initialize-plugin
	    terminate-plugin)
    (import (rnrs)
	    (archive)
	    (rfc gzip)
	    (util file)
	    (saven plugins context)
	    (saven phases)
	    (saven descriptors))

(define-record-type package-plugin-context
  (parent <saven:plugin-context>)
  (protocol (lambda (n)
	      (lambda ()
		((n "Package plugin"))))))

(define (initialize-plugin config)
  (let ((c (make-package-plugin-context)))
    (saven:plugin-context-register-phase! c 'package
     (execute-packaging c config))))

(define (execute-packaging c onfig)
  (lambda (phase-ctx)
    (define working-directory
      (saven:phase-context-working-directory phase-ctx))
    (define target-directory
      (saven:phase-context-target-directory phase-ctx))
    ;; package it the module to tar.gz (for now)
    (let ((files (find-files working-directory))
	  (dst-file (build-path target-directory
			(string-append (saven:module-descriptor-name
					(saven:phase-context-module phase-ctx))
				       ".tar.gz"))))
      (when (file-exists? dst-file) (delete-file dst-file))
      (unless (null? files)
	(let ((out (open-file-output-port dst-file (file-options no-fail))))
	  (call-with-port (open-gzip-output-port out :owner? #t)
	    (lambda (out)
	      (call-with-archive-output 'tar out
	        (lambda (aout)
		  (for-each (lambda (f)
			      (append-entry! aout (create-entry aout f)))
			    files))))))))))

(define (terminate-plugin context)
  (assert (package-plugin-context? context))
  ;; TODO terminate process if needed
  )
)
