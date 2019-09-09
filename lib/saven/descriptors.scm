#!nounbound
(library (saven descriptors)
    (export (rename (saven:module-descriptor <saven:module-descriptor>))
	    saven:module-descriptor?
	    saven:module-descriptor-name
	    saven:module-descriptor-version
	    saven:module-descriptor-dependencies
	    saven:module-descriptor-modules
	    saven:module-descriptor-build
	    saven:module-descriptor-location
	    saven:module-descriptor-source-directories
	    saven:module-descriptor-test-source-directories
	    saven:module-descriptor-parent-module
	    saven:module-descriptor-root-module

	    saven:build-file->module-descriptor
	    ;; for testing
	    make-saven:module-descriptor
	    )
    (import (rnrs)
	    (rnrs r5rs) ;; for promise
	    (sagittarius)
	    (util file)
	    (util vector)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (saven console)
	    (saven build-file))

(define-record-type saven:module-descriptor
  (fields name
	  version
	  dependencies ;; lazily initialised, at this moment it's raw sexp
	  modules
	  build
	  location
	  source-directories
	  test-source-directories
	  parent$
	  root$))
(define (saven:module-descriptor-parent-module module)
  (force (saven:module-descriptor-parent$ module)))
(define (saven:module-descriptor-root-module module)
  (force (saven:module-descriptor-root$ module)))

(define (saven:build-file->module-descriptor sav-file)
  (let-values (((dir name ext) (decompose-path sav-file)))
    (build-file->module-descriptor dir dir sav-file)))

(define (build-file->module-descriptor root-dir cur-dir sav-file)
  (define table (make-hashtable string-hash string=?))

  (saven:console-info-write "Loading saven file ~a" sav-file)
  (let ((r (%build-file->module-descriptor root-dir #f
					   cur-dir sav-file table)))
    (hashtable-set! table root-dir r)
    r))

(define (%build-file->module-descriptor root-dir parent-dir
					cur-dir sav-file table)
  (define saven (saven:read-build-file sav-file))
  (define (parent-promise) (delay (hashtable-ref table parent-dir #f)))
  (define (root-promise) (delay (hashtable-ref table root-dir #f)))
  (define (find-modules saven)
    (define (->modules name)
      (let-values (((dir file ext) (decompose-path sav-file)))
	(define new-dir (build-path dir name))
	(parameterize ((current-directory new-dir))
	  (let ((file (exists
		       (lambda (e)
			 (let ((file (build-path* (current-directory)
						  (string-append "sav." e))))
			   (and (file-exists? file) file)))
		       (saven:supported-file-extensions))))
	    (unless file
	      (assertion-violation 'saven:build-file->module-descriptor
				   "Specified module doesn't contain sav file"
				   name))
	    (%build-file->module-descriptor root-dir dir new-dir file table)))))
				     
    (cond ((assq 'modules (cdr saven)) => (lambda (m) (map ->modules (cdr m))))
	  (else '())))
  (define (find-directory dirs key default)
    (define v (and dirs (cadr dirs)))
    (cond ((and v (vector-find (lambda (s) (string=? (car s) key)) v)))
	  (else default)))
  (unless (and (pair? saven) (eq? (car saven) 'saven))
    (assertion-violation 'saven:build-file->module-descriptor
			 "Unknown file" sav-file))

  (let ((modules (find-modules saven))
	(dirs (assq 'directories saven)))
    (make-saven:module-descriptor
     (find-name saven root-dir sav-file)
     (cond ((assq 'version (cdr saven)) => cadr)
	   (else #f))
     (cond ((assq 'dependencies (cdr saven)) => cdr)
	   (else '()))
     modules
     #f
     cur-dir
     (find-directory dirs "source" '("src/main"))
     (find-directory dirs "test" '("src/test"))
     (parent-promise)
     (root-promise))))

(define (find-name saven root-dir sav-file)
  (define (extract root-dir sav-file)
    (let-values (((dir name ext) (decompose-path sav-file)))
      (if (string=? dir root-dir)
	  (assertion-violation 'saven:build-file->module-descriptor
			       "Root module must have a name element" saven)
	  (cond ((string-index-right dir #\/) =>
		 (lambda (index)
		   (substring dir (+ index 1) (string-length dir))))
		(else dir)))))

  (cond ((assq 'name (cdr saven)) => cadr)
	(else (extract root-dir sav-file))))
)
