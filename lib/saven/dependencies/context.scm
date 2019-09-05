;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven dependencies context)
    (export saven:lookup-module
	    make-saven:dependencies-context
	    saven:dependencies-context?)
    (import (rnrs)
	    (saven descriptors))

(define-record-type saven:dependencies-context
  (fields dependency modules)
  (protocol (lambda (p) (lambda (d m) (p d (travese-module m))))))

(define (travese-module module)
  (define root-module (saven:module-descriptor-root-module module))
  (define modules (make-hashtable string-hash string=?))
  (define (travese module)
    (define children (saven:module-descriptor-modules module))
    (hashtable-set! modules (saven:module-descriptor-name module) module)
    (for-each travese children))
  (travese root-module)
  modules)

(define (saven:lookup-module context name)
  (hashtable-ref (saven:dependencies-context-modules context) name #f))

)
