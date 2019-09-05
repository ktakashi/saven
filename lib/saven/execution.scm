;; -*- mode: scheme; coding: utf-8; -*-
#!nounbound
(library (saven execution)
    (export saven:execution)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (sagittarius control)
	    (srfi :1 lists)
	    (util list)
	    (saven descriptors)
	    (saven phases)
	    (saven plugins)
	    (saven dependencies))

(define +builtin-targets+
  '(
    (build . ((dep) (phases pre-build build post-build)))
    (test  . ((dep build) (phases pre-test test post-test)))
    (package . ((dep build test) (phases package)))
    (clean . ((dep) (phases clean)))
    ))

(define +phase-order+
  '(clean
    pre-build
    build
    post-build
    pre-test
    test
    post-test
    package))

(define +default-build-plugin+
  (eval '(initialize-plugin ()) (environment '(saven plugins build))))
(define +default-test-plugin+
  (eval '(initialize-plugin ()) (environment '(saven plugins test))))
(define +default-package-plugin+
  (eval '(initialize-plugin ()) (environment '(saven plugins package))))

(define (saven:execution module)
  (define-values (load-paths test-load-paths) (retrieve-load-paths module))
  (define source-directory
    (build-path (saven:module-descriptor-location module) "src"))
  (define target-directory
    ;; TODO make it configurable
    (build-path (saven:module-descriptor-location module) "target"))
  (define phase-context
    (make-saven:phase-context load-paths test-load-paths
			      (build-path source-directory "main")
			      (build-path source-directory "test")
			      target-directory
			      (build-path target-directory "main")
			      (build-path target-directory "test")
			      module))
  (define plugin-contexts
    (list +default-build-plugin+
	  +default-test-plugin+
	  +default-package-plugin+))
  ;; get plugin if we support
  (lambda (targets)
    (define phases
      (order-phases
       (apply lset-union eq?
	      (map (lambda (target)
		     (target->phases (string->symbol target))) targets))))
    (for-each (lambda (phase)
		(for-each (lambda (plugin)
			    (saven:plugin-context-execute!
			     plugin phase phase-context))
			  plugin-contexts)) phases)))

(define (order-phases phase-list)
  (filter-map (lambda (p) (and (memq p phase-list) p)) +phase-order+))

(define (retrieve-load-paths module)
  (define dependencies (saven:module-descriptor-dependencies module))
  (fold2 (lambda (d suc err)
	   (let ((path (get-path module d))
		 (scope (assq 'scope (cdr d))))
	     (if path
		 (if (and scope (equal? "test" (cadr scope)))
		     (values suc (cons path err))
		     (values (cons path suc) err))
		 (values suc err))))
	 '()  () dependencies))

(define (get-path module dependency)
  (define type (car dependency))
  (define context (make-saven:dependencies-context dependency module))
  (guard (e (else
	     ;; TODO
	     (display (condition-message e)) (newline)
	     #f))
    (eval `(retrieve-loadpath ,context ',dependency)
	  (environment '(only (rnrs) quote)
		       `(saven dependencies ,type)))))

(define (target->phases target)
  (define (resolve-dependency slot)
    (cond ((assq 'dep slot) =>
	   (lambda (d*) (append-map target->phases d*)))
	  (else '())))
  (define (resolve-phases slot)
    (cond ((assq 'phases slot) => cdr) (else '())))
  (cond ((assq target +builtin-targets+) =>
	 (lambda (slot)
	   (define deps (resolve-dependency (cdr slot)))
	   (append deps (resolve-phases (cdr slot)))))
	;; TODO error
	(else '())))

)
