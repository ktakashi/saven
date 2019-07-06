#!nounbound
(library (saven analyse)
    (export saven:analyse-descriptor)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (srfi :126 hashtables)
	    (saven descriptors))

;; DAG (well, doesn't  necessarily be but hey)
(define-record-type saven:graph
  (fields adjacencies
	  indegrees
	  vertice)
  (protocol (lambda (p)
	      (lambda (vertice edges)
		(let ((adjacencies (make-eq-hashtable))
		      (indegrees (make-eq-hashtable)))
		  (for-each (lambda (v) (hashtable-set! indegrees v 0))
			    vertice)
		  (for-each
		   (lambda (e)
		     (hashtable-update! adjacencies (saven:edge-from e)
			(lambda (v) (cons (saven:edge-to e) v)) '())
		     (hashtable-update! indegrees (saven:edge-to e)
			(lambda (v) (+ v 1)) 0))
		   (list-queue-list edges))
		  (p adjacencies indegrees vertice))))))
;; edges contains only 'to' (from is always me)
(define-record-type saven:vertix
  (fields value))
(define-record-type saven:edge
  (fields from to))

(define (saven:analyse-descriptor descriptor)
  (define graph (saven:module-descriptor->graph descriptor))
  (map saven:vertix-value (topoligical-sort graph)))

;; we need to construct DAG here
;; strategy:
;;  follow  modules on descriptor
;;  check dependency
;;   - if dependency contains defined module
;;     - then add eadge from dependency to current module
(define (saven:module-descriptor->graph descriptor)
  ;; We do lazy initialisation here so that we don't go through the
  ;; descriptor twice
  (define module-table (make-hashtable equal-hash equal?))
  (define (lookup name)
    (cond ((hashtable-ref module-table name #f))
	  (else
	   (assertion-violation 'saven:analyse-descriptor
				"module must only refer local module" name))))
  (define vertice (list-queue))
  (define edges (list-queue))
  (define dependencies-resolvers (list-queue))
  (define (walk parent module)
    (define (check-dependencies vertix module)
      (list-queue-add-back! dependencies-resolvers
	(lambda ()
	  (define dependencies
	    (filter-map (lambda (d)
			  (and (eq? 'module (car d))
			       (lookup (assq 'name (cdr d)))))
			(saven:module-descriptor-dependencies module)))
	  (for-each (lambda (v)
		      (list-queue-add-back! vertice v)
		      (list-queue-add-back! edges (make-saven:edge v module)))
		    dependencies))))
    (hashtable-set! module-table (saven:module-descriptor-name module) module)
    (let ((vertix (make-saven:vertix module)))
      (list-queue-add-back! vertice vertix)
      (when parent (list-queue-add-back! edges (make-saven:edge parent vertix)))
      (check-dependencies vertix module)
      (for-each (lambda (m) (walk vertix m))
		(saven:module-descriptor-modules module))))
  (walk #f descriptor)
  (for-each (lambda (r) (r)) (list-queue-list dependencies-resolvers))
  (make-saven:graph (list-queue-list vertice) edges))

(define (topoligical-sort graph)
  (define indegrees (hashtable-copy (saven:graph-indegrees graph) #t))
  (define adjacencies (saven:graph-adjacencies graph))
  ;; Kahn's algorithm
  ;; https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
  (define L (list-queue))
  (define S (make-list-queue
	     (filter-map
	      (lambda (v) (and (zero? (hashtable-ref indegrees v)) v))
	      (hashtable-key-list indegrees))))

  (let loop ()
    (if (list-queue-empty? S)
	(if (zero? (hashtable-sum indegrees 0 (lambda (k v acc) (+ v acc))))
	    (list-queue-list L)
	    (assertion-violation 'saven:analyse-descriptor
				 "Cyclic dependencies"
				 (map saven:module-descriptor-name
				      (hashtable-key-list indegrees))))
	(let ((m (list-queue-remove-front! S)))
	  (list-queue-add-back! L m)
	  (do ((m (hashtable-ref adjacencies m '()) (cdr m)))
	      ((null? m) (loop))
	    (hashtable-update! indegrees (car m) (lambda (v) (- v 1)) 0)
	    (unless (positive? (hashtable-ref indegrees m 0))
	      (list-queue-add-back! S (car m))))))))

)
