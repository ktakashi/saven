;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven dependencies module)
    (export retrieve-loadpath)
    (import (rnrs)
	    (util file)
	    (saven descriptors)
	    (saven dependencies context))

(define (retrieve-loadpath context dep)
  (define name (cond ((assq 'name (cdr dep)) => cadr)
		     (else (error 'module "module name is not there"))))
  (define module (saven:lookup-module context name))
  (let ((location (saven:module-descriptor-location module)))
    ;; TODO this should be taken from somewhere
    (list (build-path* location "target" "main"))))

)
