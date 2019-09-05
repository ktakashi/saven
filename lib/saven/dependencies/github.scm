;; -*- mode: scheme; coding: utf-8 -*-
#!nounbound
(library (saven dependencies github)
    (export retrieve-loadpath)
    (import (rnrs)
	    (rfc http)
	    (rfc uri)
	    (rfc gzip)
	    (binary io)
	    (archive)
	    (util file)
	    (saven dependencies context))

(define +github-url+ "https://github.com/")

(define (retrieve-loadpath context dep)
  (define name (cond ((assq 'name (cdr dep)) => cadr)
		     (else (error 'module "repository name is not there"))))
  ;; TODO default master?
  (define release (cond ((assq 'release (cdr dep)) => cadr) (else "master")))
  (define paths (cond ((assq 'paths (cdr dep)) => cdr)
		      ;; assume it's saven module then ;)
		      ;; TODO read sav file and add all modules into loadpath
		      (else '("src/main"))))
  (define url (string-append +github-url+ name "/archive/" release ".tar.gz"))
  (let ((base-path (download-archive context url)))
    (map (lambda (p) (build-path base-path p)) paths)))

(define (download-archive context url)
  (define repository-location (saven:local-repository-path context))
  (define in/out (open-chunked-binary-input/output-port))
  (define (flusher o hdr) #t)
  (define receiver (http-gzip-receiver (http-oport-receiver in/out flusher)))

  (define (local-repo-destinator e)
    (let ((name (archive-entry-name e)))
      (build-path repository-location name)))
  (let-values (((scheme specific) (uri-scheme&specific url))
	       ((server path) (url-server&path url)))
    (let-values (((s h b) (http-request 'GET server path
					:secure (string=? scheme "https")
					:receiver receiver)))
      (unless (string=? s "200")
	(error 'github-module "failed to retrieve" url))
      (set-port-position! in/out 0)
      (call-with-archive-input 'tar (open-gzip-input-port in/out :owner? #t)
	(lambda (ain)
	  (extract-all-entries ain :destinator local-repo-destinator
			       :overwrite #t))))
    repository-location))
)
