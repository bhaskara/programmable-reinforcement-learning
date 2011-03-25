;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dp/sparse-dp.lisp
;; dynamic programming algorithms for <smdp>
;; unlike the ones in dp.lisp (now removed), these don't form an adjacency matrix, and so 
;; are more suited to sparse representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package dp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; value iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod value-iteration ((m <smdp>) &key (epsilon .01) (verbose nil)
					    (init-val) (discount 1.0))
  (recover-value-fn 
   m (value-iteration (prog2
			  (when verbose (force-format t "~&Computing tabular version of smdp..."))
			  (tabular-smdp m)
			(when verbose (force-format t "done.")))
		      :epsilon epsilon :verbose verbose :discount discount
		      :init-val (awhen init-val (val-fn-to-vector it (state-set m))))))

(defmethod value-iteration ((m <tabular-smdp>) &key (epsilon .01) (verbose nil)
						    (init-val) (discount 1.0))
  (let* ((delta (1+ epsilon))
	 (num-states (num-states m))
	 (v (or init-val (make-array num-states :element-type 'float :initial-element 0.0)))
	 (new-val (make-array num-states)))
    
    (when verbose (force-format t "~&Performing value iteration"))
    (while (> delta epsilon)
      (when verbose
	(force-format t "."))
      
      ;; backup
      (dotimes (i num-states)
	(setf (aref new-val i)
	  (smdp-max-backup m v i discount)))
      
      ;; check how much v has changed by
      (setf delta (lp-dist new-val v 'infinity))
      
      ;; copy over new-val into v
      (map-into v #'identity new-val))
    
    (when verbose (force-format t "~&"))
    v))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; policy iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod policy-iteration ((m <smdp>) &key (epsilon -1) (k -1) (verbose nil) (init-pol) (discount 1.0))
  (let ((tab (prog2
		 (when verbose (force-format t "~&Computing tabular version of smdp..."))
		 (tabular-smdp m)
	       (when verbose (force-format t "done.")))))
    (multiple-value-bind (pol v)
     (policy-iteration tab :epsilon epsilon :k k :verbose verbose
		       :init-pol (awhen init-pol (convert-policy m it))
		       :discount discount)
     (values
      (recover-policy m pol)
      (recover-value-fn m v)))))

  
(defmethod policy-iteration ((m <tabular-smdp>) &key (epsilon -1) (k -1) (verbose nil) (init-pol) (discount 1.0))
  (assert (or (> epsilon 0) (> k 0)) (k epsilon) "One of epsilon or k must be provided for policy iteration.")
  (let* ((num-states (num-states m))
	 (v (make-array num-states :element-type 'float :initial-element 0.0))
	 (pol (or init-pol (make-array num-states :element-type 'fixnum :initial-element 0)))
	 (num-diffs 1))
    
    (when verbose (force-format t "~&Performing policy iteration"))
    (while (> num-diffs 0)

      (when verbose (force-format t "."))
       ;;; Policy evaluation
      (smdp-pol-eval-helper m pol v discount :epsilon epsilon :k k :verbose verbose)
      
       ;;; Policy improvement
      (setf num-diffs
	(loop 
	    for i below num-states
	    for num-actions across (num-avail-actions-vec m)
				   
	    for best-action = 
	      (do ((best nil) (best-val nil) (j 0 (incf j)))
		  ((= j num-actions) best)
		(let ((current-val (smdp-backup m v i j discount)))
		  (when (or (null best) (> current-val best-val))
		    (setf best j
			  best-val current-val))))

	    unless (terminal? m i)
	    count (not (same best-action (aref pol i))) and
	    do (setf (aref pol i) best-action)))
    
      )
    (values pol v)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; policy evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod policy-evaluation ((m <smdp>) pol &key (epsilon .01) (discount 1.0) (k -1) (verbose nil))
  (let ((tab (tabular-smdp m)))
    (recover-value-fn 
     m (policy-evaluation tab (convert-policy m pol) :k k :epsilon epsilon 
			  :discount discount :verbose verbose))))
		      

(defmethod policy-evaluation ((m <tabular-smdp>) pol &key (epsilon .01) (discount 1.0) (k -1) (verbose nil))
  (smdp-pol-eval-helper m pol (make-array (num-states m) :element-type 'float :initial-element 0.0) 
			discount :epsilon epsilon :k k :verbose verbose))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; q from v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod q-from-v ((m <smdp>) v &optional (discount 1.0))
  (let ((tab (tabular-smdp m)))
    (recover-q-fn m (q-from-v tab (val-fn-to-vector v (state-set m)) discount))))
		  

(defmethod q-from-v ((m <tabular-smdp>) v &optional (discount 1.0))
  (let ((num-states (num-states m))
	(num-actions (num-avail-actions-vec m)))
    (mapset 'vector 
	    (lambda (i)
	      (mapset 'vector (lambda (j) (smdp-backup m v i j discount))
		      (aref num-actions i)))
	    num-states)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-policy (m old-pol)
  "take a policy for smdp m and convert into a tabular policy that uses fixnums for actions"
  (let* ((states (state-set m))
	 (pol (make-array (size states))))
    (do-elements (s states pol i)
      (let ((act (avail-actions m s)))
	(unless (terminal? m s)
	  (setf (aref pol i)
	    (item-number (policy:make-choice old-pol s) act)))))))

; inner function for policy evaluation that writes value function onto array v
(defun smdp-pol-eval-helper (m pol v discount &key (epsilon -1) (k -1) (verbose nil))
  (declare (ignore verbose))
  (loop 
      with (delta new-val) 
      with num-states = (num-states m)
			 
      for u from 0
		  
      do (setf delta
	   (loop
	       for i below num-states
	       for j across pol
			     
	       unless (terminal? m i)
	       do (setf new-val (smdp-backup m v i j discount)) and
	       maximize (abs (- new-val (aref v i))) and
	       do (setf (aref v i) new-val)))
	 
	  
      while (> delta epsilon)
      while (or (< k 0) (< u k))
	    
      finally (return v)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bellman operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 

(defun smdp-max-backup (m v i disc)
  (if (aref (term-vec m) i)
      0
    (loop for j below (aref (num-avail-actions-vec m) i)
	maximize (smdp-backup m v i j disc))))

(defun smdp-backup (m v i j disc)
  (expectation (aref (aref (trans-vec m) i) j)
	       (lambda (l)
		 (+ (outcome-reward l) 
		    (* (expt disc (outcome-duration l))
		       (aref v (outcome-state l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; converting back to the original smdp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recover-value-fn (m v)
  "recover-value-fn SMDP V
Return a value function object from a vector using the state set of SMDP.  V is a vector of reals."
  (value-fn:make-tabular-value-fn v (state-set m)))

(defun recover-policy (m pol)
  "recover-policy SMDP POLICY
Convert POLICY to a <policy> defined w.r.t. SMDP.  POLICY is a vector indexed by state number, whose value at state I is the number of the chosen action at state I (numbered w.r.t the available actions at the state corresponding to I)."
  (let ((states (state-set m)))
    (policy:make-tabular-policy 
     (let ((new-pol (make-array (size states))))
       (do-elements (s states new-pol i)
	 (unless (terminal? m s)
	   (setf (aref new-pol i)
	     (item (aref pol i) (avail-actions m s))))))
     states)))

(defun recover-q-fn (m q)
  "recover-q-fn SMDP Q.
Convert Q to a <q-function> defined w.r.t SMDP.  Q is a vector indexed by state number.  The Ith value is a real vector indexed by action number (numbered w.r.t the available actions at the state corresponding to I)."
  (let ((states (state-set m)))
    (q-fn:make-tabular-q-function q states (mapset 'vector (lambda (s) (avail-actions m s)) states))))
  


