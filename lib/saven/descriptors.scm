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

	    saven:build-file->module-descriptor
	    ;; for testing
	    make-saven:module-descriptor
	    )
    (import (rnrs)
	    (sagittarius)
	    (util file)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (saven build-file))

(define-record-type saven:module-descriptor
  (fields name
	  version
	  dependencies ;; lazily initialised, at this moment it's raw sexp
	  modules
	  build
	  location))

(define (saven:build-file->module-descriptor sav-file)
  (let-values (((dir name ext) (decompose-path sav-file)))
    (build-file->module-descriptor dir dir sav-file)))

(define (build-file->module-descriptor root-dir cur-dir sav-file)
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
	      (assertion-violation 'saven:build-file->module-descriptor
				   "Specified module doesn't contain sav file"
				   name))
	    (build-file->module-descriptor root-dir dir file)))))
				     
    (cond ((assq 'modules (cdr saven)) => (lambda (m) (map ->modules (cdr m))))
	  (else '())))
  (unless (and (pair? saven) (eq? (car saven) 'saven))
    (assertion-violation 'saven:build-file->module-descriptor
			 "Unknown file" sav-file))

  (let ((modules (find-modules saven)))
    (make-saven:module-descriptor
     (find-name saven root-dir sav-file)
     (cond ((assq 'version (cdr saven)) => cadr)
	   (else #f))
     (cond ((assq 'dependencies (cdr saven)) => cdr)
	   (else '()))
     modules
     #f
     cur-dir)))

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
