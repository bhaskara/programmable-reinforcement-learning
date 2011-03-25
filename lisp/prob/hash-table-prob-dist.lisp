;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prob/hash-table-prob-dist.lisp
;; Code for treating hash tables as probability distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package prob)

(defmethod sample ((h hash-table))
  (loop
      with x = (random 1.0)
      with cumsum = 0
		    
      for item being each hash-key in h using (hash-value p)
      do (incf cumsum p)
      until (> cumsum x)
      finally (return item)))

(defmethod prob ((h hash-table) x)
  (multiple-value-bind (p pres) 
      (gethash x h)
    (if pres
	p
      (progn
	(signal 'element-not-in-domain :x x)
	0.0))))
					      
(defmethod expectation ((h hash-table) rv)
  (loop
      for item being each hash-key in h using (hash-value p)
      sum (* p (evaluate-rv rv item))))

(defmethod sample-space ((h hash-table))
  ;; inefficient
  (loop
      for k being each hash-key in h
      collect k))

(defmethod is-valid-prob-dist ((h hash-table))
  (let ((tot-prob
	 (loop
	     for v being each hash-value in h
	     when (< v 0) do (return nil)
	     sum v)))
    (and tot-prob
	 (< (abs (- tot-prob 1.0)) *prob-dist-sum-tol*))))
    

(defmethod map-prob-dist (f (h hash-table))
  (loop
      for k being each hash-key in h using (hash-value v)
      do (funcall f k v)))
