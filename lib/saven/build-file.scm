;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven build-file)
    (export saven:read-build-file
	    saven:supported-file-extensions
	    *saven:build-file-handlers*
	    +saven:default-build-file-handlers+)
    (import (rnrs)
	    (text yaml)
	    (text json)
	    (text json pointer)
	    (util file)
	    (util vector)
	    (srfi :39 parameters))

(define (saven:read-build-file file)
  (let ((ext (path-extension file)))
    (cond ((assoc ext (*saven:build-file-handlers*)) =>
	   (lambda (slot) ((cdr slot) file)))
	  ;; default sexp
	  (else (read-sexp-build-file file)))))
  
(define (read-yaml-build-file file)
  ;; allow only one YAML
  (let ((r (call-with-input-file file yaml-read)))
    (unless (null? (cdr r))
      ;; TODO proper condition
      (assertion-violation 'read-build-file "Multiple YAML documents" file))
    (adjust-build-descriptor (car r))))
(define (read-sexp-build-file file)
  (call-with-input-file file get-datum))
(define (read-json-build-file file)
  (adjust-build-descriptor (call-with-input-file file json-read)))

(define (required-json-pointer path)
  (let ((p (json-pointer path)))
    (lambda (json)
      (let ((v (p json)))
	(when (json-pointer-not-found? v)
	  ;; TODO proper condition
	  (error path "required element is missing"))
	v))))
(define (optional-json-pointer path . maybe-default)
  (define default (and (not (null? maybe-default)) (car maybe-default)))
  (let ((p (json-pointer path)))
    (lambda (json)
      (let ((v (p json)))
	(if (json-pointer-not-found? v)
	    default
	    v)))))

(define dependencies-pointer (optional-json-pointer "/dependencies" '()))
(define (remove-all-of sexp . keys)
  (vector->list
   (vector-map (lambda (s) (cons (string->symbol (car s)) (cdr s)))
	       (vector-remove (lambda (o) (member (car o) keys)) sexp))))
(define (adjust-build-descriptor sexp)
  (define (adjust p handler)
    (let ((v (p sexp)))
      (if (json-pointer-not-found? v)
	  '()
	  (handler v))))
  `(saven ,@(adjust dependencies-pointer adjust-dependencies)
	  ,@(remove-all-of sexp "dependencies")))

(define type-pointer (required-json-pointer "/type"))

(define (adjust-dependencies dependencies)
  (define (->dependency dep)
    `(,(string->symbol (type-pointer dep))
      ,@(remove-all-of dep "type")))
  `((dependencies
     ,@(map ->dependency dependencies))))

(define +saven:default-build-file-handlers+
  `(("json" . ,read-json-build-file)
    ("scm"  . ,read-sexp-build-file)
    ("yaml" . ,read-yaml-build-file)
    ("yml"  . ,read-yaml-build-file)))

(define *saven:build-file-handlers*
  (make-parameter +saven:default-build-file-handlers+))
(define (saven:supported-file-extensions)
  (map car (*saven:build-file-handlers*)))

)
	    
