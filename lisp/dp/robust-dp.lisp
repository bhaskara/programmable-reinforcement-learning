(defpackage robust-dp
  (:documentation "
Algorithms for robust dynamic programming as described in the 2004 tech report by Nilim and El Ghaoui.
- inf-horizon-rvi")
  (:use common-lisp
	lin-alg
	utils
	chi-square)
  (:export inf-horizon-rvi))


(in-package robust-dp)

(defparameter *init-val* 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun inf-horizon-rvi (count-matrix rew-matrix discount unc-level term delta &key successors)
  "inf-horizon-rvi COUNT-MATRIX REW-MATRIX DISCOUNT UNC-LEVEL TERM DELTA &key SUCCESSORS.  Implements the algorithm on page 16 of [NG04] for infinite horizon robust value iteration using the confidence set at UNC-LEVEL given observations in COUNT-MATRIX (a AxSxS array).  The reward function REW-MATRIX is assumed known.  SUCCESSORS is an array indexed by state, action where each element is either a list of possible next states or a non-list to indicate that all next states are possible.  If SUCCESSORS is not provided, all state-action pairs can transition to all states." 
  (loop
      
    
      with num-states = (array-dimension rew-matrix 0)
      with v = (make-array num-states :initial-element *init-val*)
      with succ = (or successors 
		      (make-array (array-dimensions rew-matrix)
				  :initial-element 0))
      with log-lik-gaps = 
	(map-array 
	       (lambda (s) 
		 (compute-log-lik-gap unc-level (if (listp s) (length s) num-states)))
	       succ)
      for k from 0
		 
      for sigma = (pessimistic-next-step-val v count-matrix delta log-lik-gaps term succ)
      for new-val = (backup-val rew-matrix sigma discount term)
		    
      do (debug-print 0 t "~&v = ~a" v)
		    
      until (< (lp-dist new-val v nil) delta )
      do (setf v new-val)

      
      finally (return (compute-q rew-matrix sigma discount term))))
	       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-q (rew sigma discount term)
  "Get Q from next-step expected reward"
  (let* ((dims (array-dimensions rew))
	 (ns (first dims))
	 (na (second dims))
	 (q (make-array dims :element-type 'float)))
    (dotimes (s ns q)
      (dotimes (a na)
	(setf (aref q s a) (if (aref term s) 0
			     (+ (aref rew s a) (* discount (aref sigma s a)))))))))
	       
	       

(defun backup-val (rew sigma discount term)
  "Get V from next-step expected reward"
  (let* ((dims (array-dimensions rew))
	 (ns (first dims))
	 (na (second dims))
	 (v (make-array ns)))
    (dotimes (s ns v)
      (setf (aref v s)
	(if (aref term s)
	    0
	  (loop for a below na
	      maximizing (+ (aref rew s a) 
			    (* discount (aref sigma s a)))))))))
	 

(defun pessimistic-next-step-val (neg-val counts delta log-lik-gaps term succ)
  "For each s, a compute a pessimistic next-step expected reward based on the uncertainty set from COUNTS and LOG-LIK-GAP."
  (let* ((m (loop for x across neg-val maximizing x))
	 (v (map 'vector (lambda (x) (+ 1 m (- x))) neg-val))
	 (dims (array-dimensions counts))
	 (na (first dims))
	 (ns (second dims))
	 (sigma (make-array (list ns na) :element-type 'float)))
    (dotimes (a na sigma)
      (dotimes (s ns)
	(setf (aref sigma s a)
	  (if (aref term s)
	      0
	    (multiple-value-bind (count-vector val)
		(loop
		    with succs = (let ((succs (aref succ s a)))
				   (if (listp succs)
				       succs
				     (below ns)))
		    with cvec = (make-array (length succs))
		    with vvec = (make-array (length succs))

				
		    for i in succs
		    for j from 0
			       
		    do (setf (aref cvec j) (aref counts a s i)
			     (aref vvec j) (aref v i))
		       
		    finally (return (values cvec vvec)))
	      
	      (- (1+ m) (pessimistic-backup val count-vector delta (aref log-lik-gaps s a)) ))))))))


(defun pessimistic-backup (v counts delta log-lik-gap)
  "Return the worst-case (where larger values are considered worse) expectation of V under a probability distribution belonging to the confidence set determined by COUNTS and LOG-LIK-GAP.  V is assumed to be > 0."
  
  (let ((num-samples (loop for x across counts summing x))
	(v-max (loop for x across v maximizing x)))
    (min v-max ;; due to numerical issues, the algorithm sometimes returns > v-max
	 
	 (if (or (= num-samples 0) (= (length v) 1))
	     v-max
      
    
  
	   (let* ((freq-vector (map 'vector (lambda (x) (/ x num-samples)) counts))
		  (beta-max (/ (max-log-likelihood counts) num-samples))
		  (beta (- beta-max (/ log-lik-gap num-samples)))
	 
		  (v-bar (loop for x across v for y across freq-vector summing (* x y))))
    
	     (labels 
	
		 ((sigma (mu)
		    (if (= mu v-max)
			v-max
		      (h (compute-lambda mu) mu)))
	 
		  (sigma-grad (mu)
		    (if (= mu v-max)
			(- delta)	; not correct, but should lead to the right behaviour
		      (* (sigma-grad-sign mu)
			 (lambda-grad mu))))
	 
		  (sigma-grad-sign (mu)
		    (-
		     (loop 
			 for x across freq-vector
			 for y across v
			 unless (= x 0)
			 sum (* x
				(log (/ (* (compute-lambda mu) x) (- mu y)))))
		     beta))
	 
		  (compute-lambda (mu)
		    (/ 1
		       (loop
			   for x across freq-vector
			   for y across v
			       
			   sum (/ x (- mu y)))))
	 
	 
		  (lambda-grad (mu)
		    (let ((l (compute-lambda mu)))
		      (* l l
			 (loop
			     for x across freq-vector
			     for y across v
			     for z = (- mu y)
				 
			     sum (/ x (* z z))))))
	   
	 
		  (h (l mu)
		    (when (and (> l 0) (> mu v-max))
		      (+ mu (* (1+ beta) (- l))
			 (* l
			    (loop
				for x across freq-vector
				for y across v
				    
				unless (= x 0)
				sum (* x 
				       (log (/ (* l x) (- mu y))))))))))
      
	       (declare (ignore #'sigma-grad))
	       
	       (if (or (= beta beta-max) (= v-max v-bar))
		   v-bar
		 (max v-bar
		      (loop
			  with mu- = v-max
			  with mu+ = (let ((z (exp (- beta beta-max))))
				       (/ (- v-max (* z v-bar)) (- 1 z)))
			  ;initially (debug-print 1 t "~&beta-max = ~a beta = ~a mu- = ~a and mu+ = ~a" beta-max beta mu- mu+)
						  
			   
			  for mu = (/ (+ mu+ mu-) 2)
			  while (> (- mu+ mu-) (* delta (+ 1 mu- mu+)))
			  while (> mu v-max)
			  while (> (- (sigma-grad mu+) (sigma-grad mu-)) delta)
	    

			  do 
			     ;(debug-print 1 t "~&mu- = ~a and mu+ = ~a" mu- mu+)
			    (if (> (sigma-grad-sign mu) 0)
				(setf mu+ mu)
			      (setf mu- mu))
			  finally (return (sigma mu)))))))))))
	    
	    


(defun max-log-likelihood (counts)
  "Maximum likelihood given the vector of frequencies counts."
  (loop
      with total = (loop for x across counts sum x)
      for x across counts
      when (> x 0)
      sum (* x (log (/ x total)))))



(defun compute-log-lik-gap (unc-level num-states)
  (/ (chi-square-inv-cdf (- 1 unc-level) (1- num-states)) 2))





(in-package cl-user)

	

