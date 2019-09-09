;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven phases)
    (export make-saven:phase-context
	    saven:phase-context?
	    saven:phase-context-load-paths
	    saven:phase-context-test-load-paths
	    saven:phase-context-source-directories
	    saven:phase-context-test-source-directories
	    saven:phase-context-target-directory
	    saven:phase-context-working-directory
	    saven:phase-context-test-working-directory
	    saven:phase-context-module
	    )
    (import (rnrs)
	    (saven descriptors))

(define-record-type saven:phase-context
  (fields load-paths
	  test-load-paths
	  source-directories
	  test-source-directories
	  target-directory
	  working-directory
	  test-working-directory
	  module))
)
    
