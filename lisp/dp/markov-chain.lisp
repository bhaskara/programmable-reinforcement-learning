;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dp/markov-chain.lisp
;; functions pertaining to Markov chains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package dp)

(defun exit-dist (trans-dist allowed-states &key (epsilon .01) (test #'equalp) (print-progress nil))
  "exit-dist TRANSITION-KERNEL SET &key (EPSILON .01) (TEST #'equalp) (PRINT-PROGRESS nil)

Return the exit distribution for SET.  That is, a conditional distribution P(s'|s) where s is a state in SET and s' is the first state not in SET encountered if we start at S and repeatedly sample from TRANSITION-KERNEL (which is of type [cond-prob-dist]).

EPSILON is the tolerance parameter for checking convergence.
TEST is the predicate used to test equality of elements.  If elements can be safely compared using #'eq or #'eql, specifying this will speed things up.
PRINT-PROGRESS indicates how many times to print a . indicating progress (convergence measured on log scale)

Currently, can loop forever if there's a nonzero probability of staying in the set forever.  In general, will be quite slow in very random domains with long expected trajectories."
  
  (let* ((num-states (size allowed-states))
	 (dummy-state (gensym))
	 (dists (mapset 'vector (lambda (x) (declare (ignore x)) (make-hash-table :test test)) num-states))
	 (new-dists (mapset 'vector (lambda (x) (declare (ignore x)) (make-hash-table :test test)) num-states))
	 (exit-dist (lambda (s)
		      (handler-case
			  (aref dists (item-number s allowed-states))
			(item-not-in-set ()
			  (error 'cond-dist-not-defined)))))
	 (max-diff (1+ epsilon))
	 (last-max-diff (1+ epsilon))
	 (prog-inc (/ (log epsilon) (or print-progress 1))))
    
    (flet
	((inc-prob (x p h)
	   (let ((val (gethash x h)))
	     (setf (gethash x h) (if val (incf val p) p)))))
      
      ;; initialize
      (map nil (lambda (d) (setf (gethash dummy-state d) 1.0)) dists)
      
      ;; loop until convergence
      (until (< max-diff epsilon)
	(when (and print-progress
		   (< (floor (log last-max-diff) prog-inc)
		      (floor (log max-diff) prog-inc)))
	  (format t "."))
	
	
	(setf last-max-diff max-diff)
	(setf max-diff 0)
      
	;; for each state
	(do-elements (s allowed-states nil i)
	  
	    
	  (let ((new-dist (aref new-dists i)))
	    
	    ;; reset probabilities to 0
	    (map-prob-dist (lambda (k v) (declare (ignore v)) (setf (gethash k new-dist) 0)) new-dist)
	      
	
	    ;; for each possible successor state
	    (map-prob-dist
	     #'(lambda (s2 q)
		 ;; if it's in the set, do the recursive update
		 (if (member? s2 allowed-states)
		     (let ((e-dist (aref dists (item-number s2 allowed-states))))
		       (map-prob-dist
			#'(lambda (exit-state exit-prob)
			    (inc-prob exit-state (* exit-prob q) new-dist))
			e-dist))
		   ;; otherwise, just set the prob
		   (inc-prob s2 q new-dist)))
	     (cond-dist trans-dist s))
	      
	    ;; copy the new dist over, update max-diff
	    (let ((dist (aref dists i)))
	      ;; first delete any extra elements
	      (map-prob-dist
	       (lambda (k v)
		 (unless (hash-table-has-key new-dist k)
		   (remhash k dist)
		   (maxf max-diff v)))
	       dist)
		
	      ;; copy over elements
	      (map-prob-dist
	       (lambda (k v)
		 (let ((old-val (gethash k dist)))
		   (setf (gethash k dist) v)
		   (when old-val (maxf max-diff (abs-diff old-val v)))))
	       new-dist)
	      
	      ;; check dummy state
	      (maxf max-diff (gethash dummy-state dist))))))
      

      ;; remove dummy state from each distribution and renormalize
      (map nil 
	(lambda (h) 
	  (remhash dummy-state h)
	  (let ((tot (loop for v being each hash-value in h summing v)))
	    (maphash (lambda (k v) (setf (gethash k h) (/ v tot))) h)))
	dists)
      
      ;; finally return exit dist
      exit-dist)))
      
      
      
		
		
	      
		
		
		      
			
	      
	      
			      
			      
			      
			      
	  
	  
    

