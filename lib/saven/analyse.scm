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
		   edges)
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
    (cond ((and name (hashtable-ref module-table (cdr name) #f)))
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
		      (list-queue-add-back! edges (make-saven:edge v vertix)))
		    dependencies))))
    (let ((vertix (make-saven:vertix module)))
      (hashtable-set! module-table (saven:module-descriptor-name module) vertix)
      (list-queue-add-back! vertice vertix)
      ;; TODO should we do this?
      (when parent (list-queue-add-back! edges (make-saven:edge parent vertix)))
      (check-dependencies vertix module)
      (for-each (lambda (m) (walk vertix m))
		(saven:module-descriptor-modules module))))
  (walk #f descriptor)
  (for-each (lambda (r) (r)) (list-queue-list dependencies-resolvers))
  (make-saven:graph (list-queue-list vertice) (list-queue-list edges)))

(define (topoligical-sort graph)
  (define indegrees (hashtable-copy (saven:graph-indegrees graph) #t))
  (define adjacencies (saven:graph-adjacencies graph))
  ;; Kahn's algorithm
  ;; https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
  (define L (list-queue))
  (define S (make-list-queue
	     (filter-map
	      (lambda (v) (and (zero? (hashtable-ref indegrees v)) v))
	      (saven:graph-vertice graph))))

  (let loop ()
    (if (list-queue-empty? S)
	(if (zero? (hashtable-sum indegrees 0 (lambda (k v acc) (+ v acc))))
	    (list-queue-list L)
	    (assertion-violation 'saven:analyse-descriptor
	      "Cyclic dependencies"
	      (filter-map (lambda (v)
			    (and (not (zero? (hashtable-ref indegrees v 1)))
				 (saven:module-descriptor-name
				  (saven:vertix-value v))))
			  (hashtable-key-list indegrees))))
	(let ((n (list-queue-remove-front! S)))
	  (list-queue-add-back! L n)
	  ;; The adjacencies are reverse order so make it in order
	  (do ((m (reverse (hashtable-ref adjacencies n '())) (cdr m)))
	      ((null? m) (loop))
	    (hashtable-update! indegrees (car m) (lambda (v) (- v 1)) 0)
	    (unless (positive? (hashtable-ref indegrees (car m) 1))
	      (list-queue-add-back! S (car m))))))))

)
