;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/pot-set/potential.lisp
;; operations on function potentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package pot-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (function-potential (:conc-name fpot-)
	    (:constructor create-function-potential))
  "Structure function-potential.  Create using make-function-potential."
  var-nums
  vars
  function
  args
  acc)

(defun make-function-potential (vars fn acc)
  "make-function-potential VARS FN ACC

VARS is a designator for a list of variable names.
FN is a function that takes in k arguments, corresponding to the variables listed in VARS, and returns a floating point number.
ACC is an instantiation accessor (see inst-vars package).

Returns a potential."
  (let ((names (var-names acc))
	(vars (designated-list vars)))
    (create-function-potential
     :var-nums (mapcar #'(lambda (name) 
			   (check-not-null 
			    (position name names :test #'equal)
			    "No variable called ~a in ~a" name names))
		       vars)
     :vars vars
     :function fn
     :acc acc
     :args (make-list (length vars) :initial-element nil))))


(defmethod vars ((pot function-potential))
  (fpot-vars pot))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod eval-pot ((pot function-potential) inst)
  (let ((acc (fpot-acc pot)))
    (apply (fpot-function pot)
	   (map-into (fpot-args pot) 
		     #'(lambda (i) (get-var-val acc inst i))
		     (fpot-var-nums pot)))))



(defmethod compose-potential (fn (pot function-potential))
  (make-function-potential 
   (fpot-vars pot)
   (lambda (&rest args)
     (funcall fn (apply (fpot-function pot) args)))
   (fpot-acc pot)))