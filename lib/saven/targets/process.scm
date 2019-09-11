;; -*- mode: scheme; coding: utf-8; -*-
#!nounbound
(library (saven targets process)
    (export saven:create-target-process)
    (import (rnrs))

(define (saven:create-target-process arguments)
  (lambda (phase-context)
    (write arguments) (newline)))

)
