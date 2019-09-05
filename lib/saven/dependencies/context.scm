;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven dependencies context)
    (export saven:lookup-module
	    saven:local-repository-path
	    make-saven:dependencies-context
	    saven:dependencies-context?)
    (import (rnrs)
	    (util file)
	    (saven descriptors))

(define-record-type saven:dependencies-context
  (fields dependency module modules)
  (protocol (lambda (p) (lambda (d m) (p d m (travese-module m))))))

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

(define (saven:local-repository-path context)
  (define module (saven:dependencies-context-module context))
  (define root-module (saven:module-descriptor-root-module module))
  ;; TODO move the path
  (build-path* (saven:module-descriptor-location root-module) ".sav/deps"))

)
