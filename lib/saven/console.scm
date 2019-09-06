;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven console)
    (export saven:console-info-write
	    saven:console-write)
    (import (rnrs)
	    (sagittarius))
(define (saven:console-info-write msg . args)
  (apply saven:console-write 'INFO msg args))
(define (saven:console-write level msg . args)
  (display "[") (display level) (display "] ")
  (if (null? args)
      (do-write  msg)
      (do-write (apply format msg args))))

;; TODO colouring or so?
(define (do-write msg)
  (display msg) (newline))
)
