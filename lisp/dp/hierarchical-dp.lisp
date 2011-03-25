;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dp/hierarchical-dp.lisp
;; dynamic programming operations specific to hierarchical smdps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package dp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dealing with sorted distributions over outcomes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inc-prob (l x p comp-fn)
  "inc-prob ALIST ITEM PROB COMPARISON-FN.  Assumes ALIST is a sorted association list w.r.t COMPARISON-FN and the values are real. Destructively modifies ALIST and returns a new one where the value attached to ITEM has been incremented by PROB (if ITEM did not exist before it is added).  This is used to define incf-prob. 

COMPARISON-FN should be, e.g. #'< for ascending order over fixnums."
  (if (null l)
      (list (cons x p))
    (if (funcall comp-fn x (caar l))
	(cons (cons x p) l)
      (loop
	  for l1 on l
	  for l2 on (rest l)
	  until (or (null l2) (funcall comp-fn x (caar l2)))
	  finally
	    (if (equalp x (caar l1))
		(incf (cdar l1) p)
	      (setf (cdr l1) (cons (cons x p) l2)))
	    (return l)))))

(defmacro incf-prob (l x p comp-fn)
  "incf-prob ALIST X P COMP-FN.  Sets ALIST to (INC-PROB ALIST X P)."
  `(_f inc-prob ,l ,x ,p ,comp-fn))

(defmacro incf-outcome-prob (l x p)
  `(incf-prob ,l ,x ,p #'compare-outcomes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hierarchical transition distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric hierarchical-trans-dists (m pol &key epsilon)
  (:documentation "hierarchical-trans-dists HIERARCHICAL-SMDP POLICY &key (EPSILON .01).

Uses tabular dynamic programming to compute and return three conditional distributions : 1) the same-level distribution 2) the exit distribution.

Suppose we are at a state OMEGA with level L.  

The same-level distribution consists of repeatedly sampling from the smdp transition distribution until reaching another state with level <= L. If this state has level L return it otherwise return 'nonexistent (which should only happen if OMEGA is an exit state and the next state has level L-1).  

Finally, the exit distribution consists of sampling from the transition distribution until we're at a state with level L-1 and returning it.  If L=0, return 'nonexistent.

The distributions are computed by iterating appropriate Bellman equations (note that their respective Bellman equations refer to each other)."))


(defmethod hierarchical-trans-dists ((m <hierarchical-tabular-smdp>) pol &key (epsilon .01))
  "The algorithm for tabular smdps"
  (let* ((num-states (num-states m))
	 (num-actions (num-avail-actions-vec m))
	 (trans (trans-vec m))
	 (levels (level-vec m))
	 (delta (1+ epsilon))
	 
	 ;; allocate space for new dists on each iteration
	 (pe-new (mapset 'vector 
			 (lambda (i) (make-array (aref num-actions i)))
			 num-states))
	 (pce-new (mapset 'vector 
			  (lambda (i) (make-array (aref num-actions i)))
			  num-states)))
    
    
    (labels
	((change-pce-to-pc (pce)
	   "internal function to change pce to pc by replacing transitions to parent-level states with 'nonexistent"
	   ;; loop over states
	   (loop 
	       for current-trans across pce
	       for current-level across levels
	       for num-actions-at-current-state across num-actions
				
	       ;;; loop over actions at this state
	       do (loop
		      for j below num-actions-at-current-state
		      do (let ((left-over 0) 
			       dist)
			   (setf dist
			     
			     ;; loop over outcomes
			     (loop
				 for x in (aref current-trans j)
				 for outcome = (car x)
					       
				 ;;; for outcomes at parent level, discard and increment
				 ;;; leftover probability
				 if (< (outcome-level m outcome) current-level)
				 do (incf left-over (cdr x))

				 ;;; for outcomes at current level, keep them
				 else collect x))
			   
			   ;; collect leftover probability into 'nonexistent
			   (setf (aref current-trans j)
			     (cons (cons 'nonexistent left-over) dist)))))))
			  
    
      ;; set up initial estimates of exit dist and same-level-or-exit dist
      (multiple-value-bind (pe pce)
	  (initialize-hierarchical-trans-dists m)
      
	;; loop until convergence
	(while (> delta epsilon)
	
	  ;; do a single round of backups, store in pce-new and pe-new
	  (perform-backup pce-new trans '(0 -1) '(1) pe m pol)
	  (perform-backup pe-new pce '(-1) '(0) pe m pol)
	
	  ;; update delta
	  (setf delta (max (max-l1-dist pce pce-new) (max-l1-dist pe pe-new)))
	
	  ;; copy over new vectors into pc and pe
	  (map-into pce #'identity pce-new)
	  (map-into pe #'identity pe-new)
	  )

	;; get pc from pce
	(change-pce-to-pc pce)
	
	;; return
	(values pce pe)))))


(defmethod hierarchical-trans-dists (m pol &key (epsilon .01))
  "For arbitrary SMDPs, we first convert to a tabular SMDP, then run the algorithm"
  (let ((tab (tabular-smdp m))  ;; potentially expensive
	(tab-pol (convert-policy m pol)))
    (multiple-value-bind (same-level-dist exit-dist)
	(hierarchical-trans-dists tab tab-pol :epsilon epsilon)
      (values
       (recover-outcome-dist same-level-dist m)
       (recover-outcome-dist exit-dist m)))))



(defun perform-backup (target-vec first-dist valid-level-diffs
		       diffs-requiring-backup backup-dist m pol)
  (let ((num-states (num-states m))
	(num-actions (num-avail-actions-vec m))
	(levels (level-vec m)))
    
    ;; for each state
    (dotimes (i num-states)
      (let ((current-level (aref levels i)))
	
	;; for each action at the state
	(dotimes (j (aref num-actions i))
	  (let ((l (setf (aref (aref target-vec i) j) nil)))

	    ;; for each outcome of first-dist
	    (dolist (x (aref (aref first-dist i) j))
	      (let* ((outcome (car x))
		     (prob (cdr x))
		     (i2 (outcome-state outcome))
		     (diff (- (outcome-level m outcome) current-level)))
		;; if it's already at the right level, just include it in the new dist
		(if (member diff valid-level-diffs)
		    (incf-outcome-prob l outcome prob)
		  
		  ;; else if it needs further backup do so
		  (if (member diff diffs-requiring-backup)
		      (dolist (y (aref (aref backup-dist i2)
				       (aref pol i2)))
			(incf-outcome-prob l (car y) (cdr y)))
		    
		    ;; else we have an error
		    (assert nil nil "Incorrect level difference ~a in backup" diff)))))))))))
		  
  



(defun recover-outcome-dist (dist m)
  (let ((states (state-set m)))
    (lambda (x)
      (destructuring-bind (s . a) x
	(make-rv-dist 
	 (lambda (outcome)
	   (if (symbolp outcome)
	       outcome
	   (make-outcome (item (outcome-state outcome) states)
	    (outcome-reward outcome) (outcome-duration outcome))))
	 (aref (aref dist (item-number s states))
	       (item-number a (avail-actions m s))))))))
	
	
	    
	


(defun initialize-hierarchical-trans-dists (m)
						       
  "Return initial estimates of dists over 1) next state at same or parent level 2) next state at parent level"
  
  ;; mdp parameters
  (let* ((num-states (num-states m))
	(num-actions (num-avail-actions-vec m))
	(levels (level-vec m))
	(trans (trans-vec m))
	(term (term-vec m))

	 ;; dist 1 initialize
	(pce (mapset 'vector 
		     (lambda (i) (make-array (aref num-actions i)))
		     num-states))
	 
	 ;; dist 2 initialize
	(pe (mapset 'vector 
		    (lambda (i) (make-array (aref num-actions i)))
		    num-states))
	)
    
    (labels
	((make-state-trans (i)
	   "internal function that sets up dists at state I"
	   (if (aref term i)
	       #()
	     (dotimes (j (aref num-actions i))
	       (make-state-action-trans i j))))
	 
	 (make-state-action-trans (i j)
	   "internal function that sets up dists for action J at state I."
	   (loop
	       with ce-dist = nil
	       with e-dist = nil
	       with current-level = (aref levels i)

	       ;;; iterate over outcomes for the one-step distribution
	       for x in (aref (aref trans i) j)
	       for outcome = (car x)
	       for prob = (cdr x)
	       for outcome-level = (outcome-level m outcome)
	       for level-diff = (- outcome-level current-level)

	       ;;; depending on the level, add to pce and pe
	       do (cond 
		   ((= level-diff 0)
		    (incf-outcome-prob ce-dist outcome prob))
		   ((= level-diff -1)
		    (incf-outcome-prob ce-dist outcome prob)
		    (incf-outcome-prob e-dist outcome prob))
		   (t (assert (= level-diff 1) () 
			"Level ~a of state ~a not 1 away from outcome level ~a of state ~a"
			current-level i outcome-level (outcome-state outcome))))
		  
	       finally (setf (aref (aref pce i) j) ce-dist
			     (aref (aref pe i) j) e-dist))))
	   
    
      ;; Body of main function - iterate over the states and set pe and pce
      (dotimes (i num-states)
	(make-state-trans i))
      
      ;; return
      (values pce pe))))



	

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun outcome-level (m outcome)
  "level of the destination state of this outcome"
  (level m (outcome-state outcome)))


(defun compare-outcomes (o1 o2)
  "compare-outcomes O1 O2.  O1 and O2 are outcomes with states being fixnums.  Returns t iff O1 < O2, where comparisons are done using first state, then reward, then duration."
  (if (eq o1 'nonexistent)
      t
    (if (eq o2 'nonexistent)
	nil
      (or (< (outcome-state o1) (outcome-state o2))
	  (and (= (outcome-state o1) (outcome-state o2))
	       (or (< (outcome-reward o1) (outcome-reward o2))
		   (and (= (outcome-reward o1) (outcome-reward o2))
			(< (outcome-duration o1) (outcome-duration o2)))))))))


(defun max-l1-dist (p1 p2)
  (loop
      for cd1 across p1
      for cd2 across p2
      maximize 
	(loop
	    for d1 across cd1
	    for d2 across cd2
	    maximize (l1-dist d1 d2))))



(defun l1-dist (d1 d2)
  (loop
      with l1 = d1
      with l2 = d2
      for o1 = (first l1)
      for o2 = (first l2)
      while (or l1 l2)
	    
      if (null o1)
      sum (cdr o2)
      and do (setf l2 (cdr l2))
	  
      else if (null o2)
      sum (cdr o1)
      and do (setf l1 (cdr l1))
	  
      else if (equalp (car o1) (car o2))
      sum (abs (- (cdr o1) (cdr o2)))
      and do (setf l1 (cdr l1)
		   l2 (cdr l2))
	  
      else if (compare-outcomes o1 o2)
      do (setf l1 (cdr l1))
	 
      else 
      do (setf l2 (cdr l2))))