;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven build-file)
    (export read-build-file
	    *build-file-handlers*
	    +default-build-file-handlers+)
    (import (rnrs)
	    (text yaml)
	    (text json)
	    (text json pointer)
	    (util file)
	    (srfi :39 parameters))

(define (read-build-file file)
  (let ((ext (path-extension file)))
    (cond ((assoc ext (*build-file-handlers*)) =>
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

(define dependencies-pointer (required-json-pointer "/dependencies"))
(define (adjust-build-descriptor sexp)
  (define (adjust p handler)
    (let ((v (p sexp)))
      (if (json-pointer-not-found? v)
	  '()
	  (handler v))))
  `(saven ,@(adjust dependencies-pointer adjust-dependencies)))

(define type-pointer (required-json-pointer "/type"))
(define name-pointer (required-json-pointer "/name"))
(define release-pointer (required-json-pointer "/release"))
(define scope-pointer (optional-json-pointer "/scope"))

(define (adjust-dependencies dependencies)
  (define (->dependency dep)
    `(,(string->symbol (type-pointer dep))
      (name ,(name-pointer dep))
      (release ,(release-pointer dep))
      ,@(cond ((scope-pointer dep) => (lambda (s) `((scope ,s))))
	      (else '()))))
  `((dependencies
     ,@(map ->dependency dependencies))))

(define +default-build-file-handlers+
  `(("json" . ,read-json-build-file)
    ("scm"  . ,read-sexp-build-file)
    ("yaml" . ,read-yaml-build-file)
    ("yml"  . ,read-yaml-build-file)))

(define *build-file-handlers*
  (make-parameter +default-build-file-handlers+))

)
	    
