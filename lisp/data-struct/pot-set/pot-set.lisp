;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/pot-set.lisp
;; operations on 'potential sets' (e.g. graphical models)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)


(defparameter *argops* (list (cons *max* #'argmax) (cons *min* #'argmin)))



(defun best-assignment (max-op prod-op potentials elim-order s)
  "best-assignment MAX-OP PROD-OP POTENTIALS ELIM-ORDER INST-SET

MAX-OP - an object of type operation that is either *max* or *min*
PROD-OP - the 'multiplication' operation in the underlying semiring.  Should be *times* for graphical models, and *plus* for coordination graphs.  See the operation structure type.
POTENTIALS - a list of potentials.
ELIM-ORDER - list of variables in order of elimination.  Must be exactly the set of variables that potentials depend on.
INST-SET - object of type <prod-set> that represents set of joint instantiations.

Returns
1) The best instantiation (ties are broken arbitrarily)
2) Its value"
  
  (let ((arg-op (mapping:evaluate *argops* max-op))
	(sets (sets s))
	(acc (inst-acc s)))
    (eliminate-and-backsubstitute 
     potentials elim-order s max-op prod-op
     #'(lambda (pots j)
	 #'(lambda (inst)
	     (let ((comp-set (aref sets j)))
	       (item
		(funcall arg-op comp-set
			 :key #'(lambda (val)
				  (set-var-val acc inst j val)
				  (eval-pots pots prod-op inst)))
		comp-set)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-and-backsubstitute (potentials elim-order s sum-op prod-op back-sub &aux (acc (inst-acc s)))
  "eliminate-and-backsubstitute POTENTIALS ELIM-ORDER INST-SET SUM-OP PROD-OP BACK-SUB-FN

POTENTIALS - list of potentials
ELIM-ORDER - order in which to eliminate variables
INST-SET - <prod-set> of instantiations to variables
SUM-OP - the sum operation in the underlying semiring 
PROD-OP - the product operation in the underlying semiring
BACK-SUB-FN - a function that takes in a variable number J, and a list POTS of potentials that depend on that variable, and returns a function that takes in a partial instantiation and returns a value for the variable J.  The returned function may not have any side effects on the instantiation, except that the value of variable J may be changed. 

Eliminates the variables in the given order, then uses the BACK-SUB-FN to reconstruct an instantiation.  Returns two values : the reconstructed instantiation and its value."
  (let ((rev-elim-order nil)
	(backsubstitutors nil)
	(names (var-names acc))
	(num-vars (num-vars acc))
	(init-num-pots (length potentials)))
    
    (flet ((var-number (v)
	     (check-not-null (item-number v names)
			     "There's no variable named ~a" v)))
	    
      (let ((num-pots (+ num-vars init-num-pots))
	    (current-num-pots init-num-pots)
	    pot-array depends-matrix active-pots)
      
	;; set up array of potentials, dependency matrix, active pot vector
	(setf pot-array (make-array num-pots)
	      depends-matrix (make-array (list num-pots num-vars) 
					 :element-type 'bit :initial-element 0)
	      active-pots (make-array num-pots :element-type 'bit :initial-element 0))
	(loop
	    for pot in potentials
	    for i from 0
	    do (setf (svref pot-array i) pot
		     (sbit active-pots i) 1)
	       (dolist (v (vars pot))
		 (setf (sbit depends-matrix i (var-number v)) 1)))
      
      
	(flet ((active-ref-pots (j)
		 "which active pots refer to a given variable"
		 (filter 'list num-pots
			 (lambda (i)
			   (and (bit-true (sbit active-pots i))
				(bit-true (sbit depends-matrix i j))))))
	     
	       (add-new-pot (p)
		 "do requisite bookkeeping for adding a new potential"
		 (setf (sbit active-pots current-num-pots) 1
		       (svref pot-array current-num-pots) p)
		 (dolist (v (vars p))
		   (setf (sbit depends-matrix current-num-pots (var-number v)) 1))
		 (incf current-num-pots)))
      
	  ;; eliminate variables in order
	  (dolist (v-name elim-order)
	    
	    (let* ((j (var-number v-name))
		   (pot-inds (active-ref-pots j))
		   (pots (mapcar (lambda (i) (svref pot-array i)) pot-inds))
		   (pot (apply #'multiply-and-sum-out 
			       prod-op sum-op s (item j names) 
			       pots)))
	      
	      ;; remove the old pots
	      (dolist (i pot-inds)
		(setf (sbit active-pots i) 0))
	      
	      ;; and add the new one
	      (add-new-pot pot)
	      
	      ;; push onto the backsubstitution stack both this variable
	      ;; and a closure that does a step of backsubstitution
	      (push j rev-elim-order)
	      (push (funcall back-sub pots j) backsubstitutors)
	      
	      ))
	
	
	  ;; finally start at the end and backsubstitute
	  (let ((returned-inst (create-inst acc)))
	    (mapc 
	     #'(lambda (j f)
		 (set-var-val acc returned-inst j
			      (funcall f returned-inst)))
	     rev-elim-order backsubstitutors)
	    
	    ;; return inst and value
	    (values returned-inst
		    (if rev-elim-order
			(eval-pots potentials prod-op returned-inst)
		      (op-identity sum-op)))))))))


(defun eval-pots (pots prod-op inst)
  "eval-pots POTS PROD-OP INST.  Evaluate the product of these potentials on the instantiation."
  (loop
      with prod = (op-identity prod-op)
      with f = (op-func prod-op)
	       
      for p in pots
      do (setf prod (funcall f prod (eval-pot p inst)))
      finally (return prod)))



(defun min-deficiency-elim-order (potentials vars)
  "min-deficiency-elim-order POTENTIALS VARS

POTENTIALS is a list of potentials.
VARS is the list of variable names to eliminate.

Return a list of variable names, which represents an elimination order for all variables in potentials.  The elimination order is chosen according to the minimum deficiency ordering - at each stage, eliminate the variable whose deletion results in the fewest new edges."

  ;; first convert everything to fixnums
  (let* ((n (length vars))
	 (g (make-array n)))
    (dolist (p potentials)
      (let ((nums (mapcar #'(lambda (v) (position v vars :test #'equalp)) (vars p))))
	(dolist (i nums)
	  (dolist (j nums)
	    (unless (= i j)
	      (setf (aref g i) (adjoin j (aref g i))))))))
      
    ;; data structures used
    (let ((added-edges (make-array n :initial-element nil))
	  (eliminated (make-array n :initial-element nil)))
      
      ;; helper functions
      (labels 
	  ((not-eliminated (j)
	     (not (aref eliminated j)))
	     
	   ;; active neighbours of i
	   (active-neighbours (i)
	     (append
	      (filter 'list (aref g i) #'not-eliminated)
	      (filter 'list (aref added-edges i) #'not-eliminated)))

	   ;; deficiency of a node is number of new edges needed to connect neighbours
	   (compute-deficiency (i)
	     (if (aref eliminated i)
		 'infty
	       (loop for l on (active-neighbours i)
		   sum (let ((l2 (active-neighbours (first l))))
			 (count-if (lambda (k) (not (member k l2))) (rest l))))))
	 
	   ;; eliminate
	   (eliminate (i)
	     (setf (aref eliminated i) t)
	     (mapl
	      #'(lambda (l)
		  (let* ((j (first l))
			 (l2 (active-neighbours j)))
		    (mapc
		     #'(lambda (k)
			 (unless (member k l2)
			   (push k (aref added-edges j))
			   (push j (aref added-edges k))))
		     (rest l))))
	      (active-neighbours i))))

	;; main loop
	(loop
	    repeat n
	    for best-node = (multiple-value-bind (i v n) 
				(argmin n :key #'compute-deficiency)
			      (declare (ignore i v))
			      n)
	    do (eliminate best-node)
	    collect (nth best-node vars))))))
	  

