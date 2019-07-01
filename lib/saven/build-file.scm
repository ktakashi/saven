#!nounbound
(library (saven build-file)
    (export read-build-file
	    *build-file-handlers*
	    +default-build-file-handlers+)
    (import (rnrs)
	    (text yaml)
	    (text json)
	    (util file)
	    (srfi :39 parameters))

(define (read-build-file file)
  (let ((ext (path-extension file)))
    (cond ((assoc ext (*build-file-handlers*)) =>
	   (lambda (slot) ((cdr slot) file)))
	  ;; default sexp
	  (else (read-sexp-build-file file)))))
  
(define (read-yaml-build-file file)
  (call-with-input-file file yaml-read))
(define (read-sexp-build-file file)
  (call-with-input-file file get-datum))
(define (read-json-build-file file)
  (call-with-input-file file json-read))
    
(define +default-build-file-handlers+
  `(("json" . ,read-json-build-file)
    ("scm"  . ,read-sexp-build-file)
    ("yaml" . ,read-yaml-build-file)
    ("yml"  . ,read-yaml-build-file)))

(define *build-file-handlers*
  (make-parameter +default-build-file-handlers+))

)
	    
