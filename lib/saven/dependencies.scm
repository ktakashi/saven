;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven dependencies)
    (export saven:lookup-module
	    make-saven:dependencies-context
	    saven:dependencies-context?)
    (import (saven dependencies context)))
