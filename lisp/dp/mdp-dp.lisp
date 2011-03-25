(in-package dynamic-programming)


(defmethod value-iteration ((m <mdp>) &key (epsilon .01) (verbose nil) (init-val) (discount 1.0))
  (declare (ignore verbose))
  
  (let* ((states (state-set m))
	 (num-states (size states))
	 (trans (transition-matrix m))
	 (rew (reward-matrix m))
	 (delta (1+ epsilon))
	 
	 ;; instead of allocating a new vector on each iteration, allocate two at the beginning (we 
	 ;; need two so we can measure the distance between successive iterates)
	 (v (aif init-val
		 (val-fn-to-vector it states)
		 (make-array num-states :element-type 'float :initial-element 0.0)))
	 (new-val (make-array num-states)))
    
    (while (> delta epsilon)
      
      ;; backup
      (dotimes (i num-states)
	(setf (aref new-val i)
	  (max-backup v i trans rew discount)))
      
      ;; check how much v has changed by
      (setf delta (lin-alg:lp-dist new-val v 'infinity))
      
      ;; copy over new-val into v
      (map-into v #'identity new-val))
    
    ;; return the value function (in tabular form)
    (value-fn:make-tabular-value-fn v states)))



(defmethod policy-iteration ((m <mdp>) &key (epsilon -1) (k -1) (verbose nil) (init-pol) (discount 1.0))
  
  (assert (or (> epsilon 0) (> k 0)) (k epsilon) "One of epsilon or k must be provided for policy iteration.")
  (let* ((states (state-set m))
	 (actions (action-set m))
	 (trans (transition-matrix m))
	 (rew (reward-matrix m))
	 (num-states (size states))
	 (action-inds (below (size actions)))
	 (v (make-array num-states :element-type 'float :initial-element 0.0))
	 (pol (aif init-pol
		   (pol-to-vector it states)
		   (make-array num-states :element-type 'fixnum :initial-element 0)))
	 (num-diffs 1))
    
    (while (> num-diffs 0)
		
       ;;; Policy evaluation
      (policy-evaluation-inner trans rew pol v discount :epsilon epsilon :k k :verbose verbose)
	 
       ;;; Policy improvement
      (setf num-diffs
	(loop 
	    for i below num-states
	    for best-action = 
	      (argmax (mapcar #'(lambda (a) (backup v i a trans rew discount)) action-inds))
	    counting (/= best-action (aref pol i))
	    do (setf (aref pol i) best-action)))
    
      (when verbose (format t "~&")))
	    
    (values (policy:make-tabular-policy (map 'vector (lambda (x) (item x actions)) pol) states)
	    (value-fn:make-tabular-value-fn v states))))


(defmethod policy-evaluation ((m <mdp>) pol &key (epsilon .01) (discount 1.0))
   (let ((states (state-set m)))
     (value-fn:make-tabular-value-fn 
      (policy-evaluation-inner 
       (transition-matrix m)
       (reward-matrix m)
       (pol-to-vector pol states )
       (make-array 
	(size states)
	:element-type 'float 
	:initial-element 0.0) 
       discount
       :epsilon epsilon)
      states)))



(defmethod q-from-v ((m <mdp>) v &optional (discount 1.0))
  (let* ((states (state-set m))
	 (actions (action-set m))
	 (num-states (size states))
	 (num-actions (size actions))
	 (trans (transition-matrix m))
	 (rew (reward-matrix m))
	 (q (make-array num-states))
	 (val-vec (val-fn-to-vector v states)))
    (dotimes (i (size states))
      
      (let ((row (setf (aref q i) (make-array num-actions))))
	(dotimes (j (size actions))
	  (setf (aref row j)
	    (backup val-vec i j trans rew discount)))))
    (q-fn:make-tabular-q-function q states 
				  (loop 
				      with a = (make-array num-states)
				      for i below num-states
				      do (setf (aref a i) actions)
				      finally (return a)))))


	 

(defun max-backup (v i trans rew disc)
  (loop
      for a below (array-dimension trans 1)
      maximizing (backup v i a trans rew disc)))



(defun backup (v i a trans-matrix rew-matrix disc)
  (loop
      with num-states = (array-dimension trans-matrix 2)
      for j below num-states
      summing (* (aref trans-matrix i a j)
		 (+ (aref rew-matrix i a j)
		    (* disc (aref v j))))))







(in-package common-lisp-user)    


