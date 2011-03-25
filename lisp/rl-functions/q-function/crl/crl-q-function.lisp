;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/q-function/crl-q-function.lisp
;; Implements <crl-q-function>, for coordinated relational linear Q-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage crl-q-function
  (:documentation "Package for coordinated relational linear Q-functions.
See also the q-function package.

Types
-----
<crl-q-function>

Accessors
---------
set-temps
ids
set-ids
choice-fn
set-choice-fn
debug-mode

Features
--------
make-feature
vars - accessor
func - accessor

Creating features and feature templates
---------------------------------------
compose-feature
compose-feature-template
bin-values

Evaluating
----------
weights
feature-vector
evaluate
")



  (:nicknames crlq)
  (:use q-fn
	cl
	set
	pot-set
	utils)
  (:export
   <crl-q-function>
   make-feature
   vars
   func
   set-temps
   set-ids
   choice-fn
   set-choice-fn
   ids
   debug-mode
   
   compose-feature
   compose-feature-template
   bin-values
   
   weights
   evaluate
   feature-vector))

(in-package crl-q-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-feature (vars func acc)
  "make-feature VARS FUNC INST-ACCESSOR.
  
VARS - designator for a list of variable names.
FUNC - function
INST-ACCESSOR - inst-accessor.  See inst-vars package.

Return a feature that depends on VARS and applies FUNC to their values.  INST-ACCESSOR is used to access variable values.  The returned object is used internally by the CRL Q-function operations."
  (make-function-potential vars func acc))
  
  
(defclass <crl-q-function> (<q-function>)
  ((dim :type fixnum :reader dim :writer set-dim)
   (feature-templates :type vector :initarg :feature-templates :reader temps :writer set-temps)
   (feature-ids :type vector :reader ids :initarg :ids :writer set-ids)
   (weights :type vector :reader weights :initarg :weights :writer set-weights)
   (choice-fn :type function :reader choice-fn :initarg :choice-fn :writer set-choice-fn)
   (debug-mode :type boolean :accessor debug-mode :initform nil :initarg :debug-mode))
   
  (:documentation "Class for coordinated relational linear Q-functions.

:weights - vector of weights corresponding to the feature templates.  By default, all 0's.
:ids - vector of ids for each feature template.  By default is #(0 1 2 ... dim-1)
:choice-fn - function that takes in a state, and returns a <prod-set> representing the set of available choices.
:debug-mode - if true, when maximizing q-function exhaustive maximization is also used, and compared with the answer of coordination graph maximization.  Nil by default.

Must also provide at most one of the following
:template-descs - List of template descriptions.  Each template description is a designator for a list of functions.  The functions are the feature templates - each one must take in a joint state omega, and output a list of applicable features at omega.  See the description of the feature structure-type.
:feature-templates - vector of feature templates

A coordinated Q-function is designed for domains in which the choices are structured objects, e.g. vectors, consisting of many components.  So, when looking for the best choice, exhaustive enumeration of all choice is impractical. The Q-function is approximated as a weighted combination of 'feature templates'.  A feature template is either a constant or a function that takes in a state and returns an object that designates a list of features, each of which is either a number or an object created using make-feature.  The value of the feature template at a state-action is the sum of the corresponding features."))


(defmethod copy-into progn ((q <crl-q-function>) (q2 <crl-q-function>))
  (reinitialize-instance q2 :feature-templates (temps q) :ids (ids q) 
			 :weights (clone (weights q)) :choice-fn (choice-fn q)))
  

(defmethod shared-initialize :around ((q <crl-q-function>) slots 
				      &rest args 
				      &key (template-descs nil temp-descs-supplied)
					   (feature-ids 
					    (coerce (below (length template-descs)) 'vector)))

  ;; parse the :template-descs argument if it's supplied, and call the next method
  (if temp-descs-supplied
      (apply #'call-next-method
	     q slots
	     :ids
	     (loop
		 with v = (make-array 0 :adjustable t :fill-pointer 0)
		 for id across feature-ids
		 for d in template-descs
		 for n = (if (listp d) (length d) 1)
		 if (= n 1)
		 do (vector-push-extend id v)
		 else
		 do (dotimes (i n)
		      (vector-push-extend (intern-compound-symbol id (format nil "~a" i)) v))
		 finally (return v))
	     :feature-templates
	     (coerce
	      (mapcan #'designated-list template-descs)
	      'vector)
	     args)
    (call-next-method))
  
  ;; when the feature-templates field has been set, use it to set some of the other fields
  ;; to default values
  (when (slot-boundp q 'feature-templates)
    (let ((dim (length (temps q))))
      (set-dim dim q)
      (set-if-unbound 'feature-ids q (coerce (below dim) 'vector))
      (set-if-unbound 'weights q (make-array dim :element-type 'float :initial-element 0.0)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating Q
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod evaluate ((q <crl-q-function>) omega u)
  (loop
      for temp across (temps q)
      for w across (weights q)
      sum (* w (evaluate-temp temp omega u))))


(defun evaluate-temp (temp omega u)
  "evaluate-temp FEATURE-TEMPLATE OMEGA U.

FEATURE-TEMPLATE is either a function or a number (which represents the function that always returns that number).
OMEGA is a state.
U is a (joint) choice.

Apply FEATURE-TEMPLATE to OMEGA, apply every feature in the list designated by the return value to U using evaluate-feature."
  (etypecase temp
    (function (sum-over (designated-list (funcall temp omega))
			#'(lambda (feature) (evaluate-feature feature u))))
    (real temp)))


(defun evaluate-feature (feature u)
  "evaluate-feature FEATURE U.
FEATURE is a feature (of type potential)
U is a (joint choice)."
  (eval-pot feature u))


(defun feature-vector (q omega u)
  "feature-vector Q OMEGA U"
  (map 'vector #'(lambda (temp) (evaluate-temp temp omega u)) (temps q)))
		   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finding best choice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod best-choice ((q <crl-q-function>) omega)
  (let ((potentials (get-weighted-potentials q omega))
	(choices (choices q omega)))
    
    (multiple-value-bind (i v)
	(best-assignment *max* *plus* potentials 
			 (min-deficiency-elim-order 
			  potentials 
			  (inst-vars:var-names (prod-set:inst-acc choices)))
			  choices)
      (when (debug-mode q)
	(multiple-value-bind (i2 v2) (call-next-method)
	  (declare (ignore i2))
	  (assert (< (abs-diff v v2) .01) ()
	    "In debug mode for CRLQ at state ~a.  Coordination graph algorithm gave answer ~a, which is different from exhaustive algorithm's answer ~a." omega v v2)))
      (values i v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boltzmann distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(defmethod boltzmann-dist (q omega temp)
  (let ((potentials (get-weighted-potentials q omega))
	(choices (choices q omega)))
    (boltzmann-gm 
     potentials 
     (min-deficiency-elim-order 
      potentials (inst-vars:var-names (prod-set:inst-acc choices)))
     choices temp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other q-function methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun get-weighted-potentials (q omega)
  (apply #'nconc
	 (map 'list
	   #'(lambda (temp w)
	       (flet ((mult (x) (* x w)))
		 (etypecase temp
		   (number (list (mult temp)))
		   (function
		    (mapcar #'(lambda (f) (compose-feature #'mult f))
			    (designated-list (funcall temp omega)))))))
	   (temps q) (weights q))))

(defmethod choices ((q <crl-q-function>) omega)
  (funcall (choice-fn q) omega))

(defmethod update ((q <crl-q-function>) omega u target eta)
  (let* ((v (feature-vector q omega u))
	 (w (weights q))
	 (actual
	  (loop
	      for f across v
	      for weight across w
	      sum (* f weight)))
	 (diff (- target actual)))
    
    (dotimes (i (length w))
      (incf (aref w i) (* diff eta (aref v i))))))
      
(defmethod policy:print-advice ((adv crlq:<crl-q-function>) omega choices str)
  (format str "~&Weights : ~a" (crlq:weights adv))
  (do-elements (u choices)
    (format str "~&~%Choice : ~a~&Features : ~a~&Q-value : ~a" 
	    u
	    (sparsify (crlq:feature-vector adv omega u) :name-fn (lambda (i) (aref (crlq:ids adv) i)))
	    (crlq:evaluate adv omega u)))
  (multiple-value-bind (u v)
      (q-fn:best-choice adv omega)
    (format str "~&~%Best choice is ~a with value ~a" u v)))
  
