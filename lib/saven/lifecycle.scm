;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle)
    (import (rnrs)
	    (saven descriptor))

(define (saven:lifecycle sav-file . targets)
  (define descriptor (saven:build-file->module-descriptor sav-file))
  descriptor)
)
	    
