;;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven)
    (export saven:build-file->module-descriptor
	    saven:analyse-descriptor
	    saven:lifecycle)
    (import (saven descriptors)
	    (saven analyse)
	    (saven lifecycle)))

