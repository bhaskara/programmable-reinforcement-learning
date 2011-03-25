;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/potential.lisp
;; operations on potentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)


(defgeneric vars (potential)
  (:documentation "vars POTENTIAL.  Return list or vector of variables to which this potential refers."))

(defgeneric eval-pot (pot inst)
  (:documentation "eval-pot POTENTIAL INSTANTIATION

POTENTIAL is a potential.
INSTANTIATION is a joint instantiation.

Signals an error if any variable referred to by POT is uninstantiated."))
	   

(defgeneric multiply (op s pot &rest pots)
  (:documentation
   "multiply OP INST-SET &rest POTENTIALS

OP represents a multiplication operation
INST-SET is the set of joint instantiations to all the variables (not just the ones referred to by POTENTIALS)
POTENTIALS is a list of potentials

Returns a new potential which depends on the union of the variables in the potentials, and which represents the product of their functions.")
  (:method (op s pot &rest p2 &aux (pots (cons pot p2)))
	   (let ((vars (unite (mapcar #'vars pots))))
	     (make-tabular-potential s vars
				     (lambda (inst)
				       (loop
					   with prod = (op-identity op)
					   for p in pots
					   do (setf prod (operate op prod (eval-pot p inst)))
					   finally 
					     (return prod)
					     ))))))


(defgeneric multiply-and-sum-out (mult-op sum-op s v &rest pots)
  (:documentation
   "multiply-and-sum-out MULT-OP SUM-OP INST-SET VAR-NAME &rest POTENTIALS

MULT-OP, SUM-OP are of type operation
INST-SET is the set of joint instantiations to all the variables (not just the ones referred to by POTENTIALS)
VAR-NAME is the name of one of the variables
POTENTIALS is a list of potentials.

Returns a new potential which depends on the union of the variables in the potentials minus VAR-NAME.")
  (:method (mult-op sum-op s v &rest pots)
	   (let ((pot-vars (mapcar #'vars pots))
		 (v-subspace (make-subspace s (list v) t))
		 (acc (inst-acc s)))
	     (assert (every (lambda (pv) (member v pv :test #'equal)) pot-vars)
		 () "Not all variable lists ~a contain variable ~a" pot-vars v)
	     (let ((vars (delete v (unite pot-vars) :test #'equal))
		   (num (get-var-num acc v))
		   (vals (mapset
			  'list
			  (lambda (i) (get-var-val-by-name acc i v))
			  v-subspace)))
	       (make-tabular-potential 
		s vars
		(lambda (inst)
		  (loop
		      with total = (op-identity sum-op)
		      for v-val in vals
		      do (set-var-val acc inst num v-val)
			 (setf total 
			   (operate 
			    sum-op total
			    (loop
				with prod = (op-identity mult-op)
				for p in pots
				do (setf prod 
				     (operate mult-op prod 
					      (eval-pot p inst)))
				finally (return prod))))
		      finally (return total))))))))



(defgeneric compose-potential (fn pot)
  (:documentation "compose-potential FUNCTION POTENTIAL.  Return a new potential, which applies FUNCTION to the result of POTENTIAL."))

						       
   

  

  
  