;; -*- mode: scheme; coding: utf-8; -*-
#!nounbound
(library (saven targets process)
    (export saven:create-target-process)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :1 lists)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))

(define (saven:create-target-process arguments)
  (define arguments-referers
    (cond ((assq 'arguments arguments) =>
	   (lambda (a) (map saven:parse-process-arguments (cdr a))))
	  (else '())))
  (lambda (phase-context)
    (define arguments
      (append-map (lambda (ref) (ref phase-context)) arguments-referers))
    (write arguments) (newline)))

(define (saven:parse-process-arguments s)
  (define lseq (generator->lseq (string->generator s)))
  (let-values (((s v nl) ($argument lseq)))
    (if (parse-success? s)
	v
	s)))

(define (make-single-ref var . maybe-joint)
  (define joint (if (null? maybe-joint) " " (car maybe-joint)))
  (lambda (env) (string-append "$" var "$")))

(define (make-array-ref var . maybe-prefix&suffix)
  (define prefix (if (null? maybe-prefix&suffix) "" (car maybe-prefix&suffix)))
  (define suffix (if (or (null? maybe-prefix&suffix)
			 (null? (cdr maybe-prefix&suffix)))
		     ""
		     (cadr maybe-prefix&suffix)))
  (lambda (env) (list (string-append "array:" var))))


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
