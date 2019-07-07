;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven lifecycle)
    (export saven:lifecycle)
    (import (rnrs)
	    (saven descriptors)
	    (saven analyse))

(define (saven:lifecycle sav-file . targets)
  (define descriptor (saven:build-file->module-descriptor sav-file))
  (define sorted-modules (saven:analyse-descriptor descriptor))
  (display (map saven:module-descriptor-name sorted-modules)) (newline)
  sorted-modules)
)
	    
