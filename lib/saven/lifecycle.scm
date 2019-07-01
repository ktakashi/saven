;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle)
    (import (rnrs)
	    (sagittarius)
	    (util file)
	    (srfi :39 parameters)
	    (saven build-file))


(define-record-type saven:module-descriptor
  (fields dependencies
	  modules
	  build))

(define (saven:lifecycle sav-file . targets)
  (define descriptor (build-file->module-descriptor sav-file))
  descriptor)

(define (build-file->module-descriptor sav-file)
  (define saven (saven:read-build-file sav-file))
  (define (find-modules saven)
    (define (->modules name)
      (let-values (((dir file ext) (decompose-path sav-file)))
	(parameterize ((current-directory dir))
	  (let ((file (exists
		       (lambda (e)
			 (let ((file (build-path* (current-directory)
						  name
						  (string-append "sav." e))))
			   (and (file-exists? file) file)))
		       (saven:supported-file-extensions))))
	    (unless file
	      (assertion-violation 'saven:lifecycle
				   "Specified module doesn't contain sav file"
				   name))
	    (build-file->module-descriptor file)))))
				     
    (cond ((assq 'modules (cdr saven)) => (lambda (m) (map ->modules (cdr m))))
	  (else #f)))
  (unless (and (pair? saven) (eq? (car saven) 'saven))
    (assertion-violation 'saven:lifecycle "Unknown file" sav-file))

  (let ((modules (find-modules saven)))
    (make-saven:module-descriptor
     (assq 'dependencies (cdr saven))
     modules
     #f)))
)
	    
