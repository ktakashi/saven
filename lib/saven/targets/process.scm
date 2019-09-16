;; -*- mode: scheme; coding: utf-8; -*-
#!nounbound
(library (saven targets process)
    (export saven:create-target-process)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius)
	    (sagittarius generators)
	    (sagittarius process)
	    (saven phases)
	    (saven descriptors)
	    (saven lifecycle)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))

(define (saven:create-target-process arguments)
  (define arguments-referers
    (cond ((assq 'arguments arguments) =>
	   (lambda (a) (map saven:parse-process-arguments (cdr a))))
	  (else '())))
  (define name (cadr (assq 'name arguments)))
  (define background? (cond ((assq 'background arguments) => cadr) (else #f)))
  (define workdir
    (cond ((assq 'work_directory arguments) =>
	   (lambda (slot) (saven:parse-process-arguments (cadr slot))))
	  (else #f)))
  (lambda (phase-context)
    (define env (make-saven:argument-environment phase-context))
    (define arguments
      (append-map (lambda (ref) (ref env)) arguments-referers))
    (define work-dir (or (and workdir (car (workdir env)))
			 (saven:phase-context-working-directory phase-context)))
    (parameterize ((current-directory work-dir))
      (let ((p (create-process name arguments
			       :stdout (standard-output-port)
			       :stderr (standard-error-port)
			       :detach? background?)))
	(if background?
	    (saven:push-cleanup!
	     (lambda ()
	       (when (process-active? p)
		 (or (process-wait p :timeout 0)
		     (process-kill p :children? #t)))))
	    (process-wait p))))))

(define (saven:parse-process-arguments s)
  (define lseq (generator->lseq (string->generator s)))
  (let-values (((s v nl) ($argument lseq)))
    (if (parse-success? s)
	v
	s)))

(define (make-single-ref var . maybe-joint)
  (define joint (if (null? maybe-joint) " " (car maybe-joint)))
  (lambda (env)
    (let ((v (saven:argument-environment-ref env var)))
      (if (pair? v)
	  (string-join v joint)
	  v))))

(define (make-array-ref var . maybe-prefix&suffix)
  (define prefix (if (null? maybe-prefix&suffix) "" (car maybe-prefix&suffix)))
  (define suffix (if (or (null? maybe-prefix&suffix)
			 (null? (cdr maybe-prefix&suffix)))
		     ""
		     (cadr maybe-prefix&suffix)))
  (define (make-value v)
    (string-append prefix v suffix))
  (lambda (env)
    (let ((v (saven:argument-environment-ref env var)))
      (if (pair? v)
	  (map make-value v)
	  (list (make-value v))))))

;; simple environment... for now
(define (make-saven:argument-environment phase-context)
  (define table (make-hashtable string-hash string=?))
  (define (add key accessor)
    (hashtable-set! table key (accessor phase-context)))
  (define (->module-accessor accessor)
    (lambda (ctx)
      (accessor (saven:phase-context-module ctx))))
  ;; pre-define variables
  (add "load-paths" saven:phase-context-load-paths)
  (add "test-load-paths" saven:phase-context-test-load-paths)
  (add "source-directories" saven:phase-context-source-directories)
  (add "test-source-directories" saven:phase-context-test-source-directories)
  (add "target-directory" saven:phase-context-target-directory)
  (add "output-directory" saven:phase-context-working-directory)
  (add "test-output-directory" saven:phase-context-test-working-directory)
  (add "project-directory" (->module-accessor saven:module-descriptor-location))
  ;; TODO add module properties
  table)
(define (saven:argument-environment-ref env key)
  (hashtable-ref env key ""))


;; variable ref (EBNF)
;; argument = string | array-ref | single-ref
;; array-ref = '@{', variable, [',' prefix, [',' suffix ]], '}';
;; single-ref = '${', varialbe, [',' joint], '}';
;; variable = non-quoted-string
;; prefix = string
;; suffix = string
;; joint = string
;; string = non-quoted-string | quoted-string
;; non-quoted-string = { letter | digit | '-' | '_' }
;; quoted-string = '"', { any } , '"'

(define non-quoted-set
  (char-set-intersection
   char-set:ascii
   (char-set-union char-set:letter+digit
		   ;; TODO think about it
		   (string->char-set "!@#$%^&*()-+=[]\\|':;?/.<>"))))
(define $non-quoted-string
  ($do (c* ($many ($char-set-contains? non-quoted-set)))
       ($return (list->string c*))))
(define $variable $non-quoted-string)

;; for now
(define $string
  ($do (c* ($many ($seq ($peek ($not ($token "@{")))
			($peek ($not ($token "${")))
			$any) 1))
       ($return (let ((s (list->string c*)))
		  (lambda (env) s)))))

(define $joint ($seq ($eqv? #\,) $non-quoted-string))
(define $single-ref
  ($do (($token "${"))
       (v $variable)
       (j ($optional $joint))
       (($eqv? #\}))
       ($return (if j (make-single-ref v j) (make-single-ref v)))))

(define $suffix $joint)
(define $prefix $joint)
(define $prefix/suffix
  ($do (p ($optional $prefix #f))
       (s ($optional $suffix #f))
       ($return (cond ((and p s) (list p s))
		      ((or p s) => list) ;; must only be prefix though
		      (else '())))))

(define $array-ref
  ($do (($token "@{"))
       (v $variable)
       (p&s ($optional $prefix/suffix '()))
       (($eqv? #\}))
       ($return (apply make-array-ref v p&s))))

(define $argument
  ($or $array-ref ;; this can't be concatenated
       ;; argument can be ${foo}/bar
       ($do (r* ($many ($or $single-ref $string)))
	    ($return (let ((ref
			    (fold-left (lambda (acc r)
					 (lambda (env)
					   (string-append (acc env) (r env))))
				       (lambda (env) "") r*)))
		       ;; make my life easier...
		       (lambda (env) (list (ref env))))))))

)
