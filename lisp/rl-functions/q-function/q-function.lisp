(defpackage q-function
  (:documentation "Package q-function (q-fn).  Code and types related to Q-functions of MDPs.

Types
-----
<q-function>
<approx-q-function> - Subtype of <q-function>
<sum-q-function> - Subtype of <q-function>
<env-q-function> - Subtype of <approx-q-function>

Variables
---------
*num-decimal-places-to-print*

Ways to create q-functions
--------------------------
make-tabular-q-function
sum-q-functions

Methods that can be called on a q-function
------------------------------------------
best-choice
boltzmann-dist
evaluate
update
reset

Additional operations relating to approx-q-functions
----------------------------------------------------
fn-approx
featurizer
make-q-featurizer
get-designated-q-feature
print-approx-q-function-readably


Subtypes of <q-function> must implement
---------------------------------------------------------
choices

Subtypes of <approx-q-function> must implement
----------------------------------------------
featurize

Other methods for <sum-q-function>
----------------------------------
evaluate-comps

Conditions, restarts, restart functions
---------------------------------------
unknown-state
unknown-state-action
first-choice
use-default-val-0
choose-randomly
")


  (:nicknames q-fn)
  (:use common-lisp
	utils
	set
	value-fn)
  (:import-from policy
		unknown-state
		unknown-state-action)
  (:export <q-function>
	   <approx-q-function>
	   <sum-q-function>
	   <env-q-function>
	   best-choice
	   value
	   boltzmann-dist
	   evaluate
	   update
	   reset
	   fn-approx
	   featurizer
	   choices
	   featurize
	   make-q-featurizer
	   get-designated-q-feature
	   print-approx-q-function-readably
	   make-tabular-q-function
	   sum-q-functions
	   evaluate-comps
	   unknown-state-action
	   unknown-state
	   first-choice
	   use-default-val-0
	   choose-randomly
	   ))


(in-package q-function)


(defclass <q-function> (<value-fn>)
  ()
  (:documentation "Represents the Q-function (action-value function) in a sequential decision process.  
"))


  

(defgeneric best-choice (q-fn omega)
  (:documentation "maximizing-choice Q-FUNCTION STATE.  Return two values - the choice that maximizes the q-function at this state, and the Q-value of that choice.  Choices whose evaluation results in a 'unknown-state-action condition are ignored.  If all choices are unknown, then best-choice itself should signal an error of type unknown-state-action.  An around method at the top level provides the restarts first-choice and random-choice for this case, which return the first choice or a random choice, and assume the Q-value is 0.  If the state itself is unknown, signals an error of type 'unknown-state")
  (:method :around (q-fn omega)
	   (let ((ch (choices q-fn omega)))
	     (restart-case (call-next-method)
	       (first-choice ()
		 (values (set:item 0 ch) 0))
	       (random-choice ()
		 (values (set:item (random (set:size ch)) ch) 0))))))
	

(defgeneric evaluate (q-fn omega u)
  (:documentation "evaluate Q-FN OMEGA U.  Evaluate a Q-function on a state and action.   Might signal an error of type unknown-state-action.  The top-level around method provides a use-value restart, in case higher-level code wants to provide a default value.")
  (:method :around (q-fn omega u)
	   (declare (ignore q-fn omega u))
	   (restart-case (call-next-method)
	     (use-value (val)
		 :report "Provide a value to use this time as the Q-value."
		 :interactive (lambda () 
				(format t "~&Enter a Q-value.  ")
				(list (read)))
	       val))))
				      

(defgeneric boltzmann-dist (q-fn omega temp)
  (:documentation "boltzmann-dist Q-FUNCTION OMEGA TEMPERATURE.  Return an object of type [prob-dist], which represents the Boltzmann distribution over actions assuming the given temperature (where the ``energy'' of an action is its negative Q-value.   Might signal an error of type unknown-state-action.

The TEMPERATURE argument must be either a nonnegative real number or the symbol 'infty (from the utils package).  A temperature of 0 will return some distribution over the maximizing choices.  A temperature of 'infty represents the uniform distribution over all choices."))

;; todo, this probably doesn't belong at the top level
(defgeneric update (q-fn omega u target eta)
  (:documentation "update Q OMEGA U TARGET STEP-SIZE.  Make the Q-value at OMEGA,U be a bit ``closer'' to TARGET."))

(defgeneric reset (q-fn &optional theta)
  (:documentation "reset Q &optional THETA.  Reset the parameters of the Q-function.  The new values of the parameters may be optionally provided using THETA."))

(defgeneric sum-q-functions (q-fn &rest q-fns)
  (:documentation "sum-q-functions q-fn &rest q-fns.  Return a new <q-function> object that is the sum of the individual Q-functions.  The new Q-function need not support update, but it should support reset, evaluate, best-choice, clone, and boltzmann-dist."))
  

(defmethod value ((q-fn <q-function>) omega)
  (multiple-value-bind (u val)
      (best-choice q-fn omega)
    (declare (ignore u))
    val))


(defmethod clone ((q1 <q-function>))
  ;; should this go higher in the class hierarchy?
  (let ((q2 (make-instance (class-of q1))))
    (copy-into q1 q2)
    q2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default methods for best choice and boltzmann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod best-choice ((q-fn <q-function>) omega)

  (let ((choices (choices q-fn omega)))
    (let ((known-choices (make-array (size choices) :fill-pointer 0))
	  (vals (make-array (size choices) :fill-pointer 0))
	  (done? nil)
	  (iter (iterator choices))
	  ch)
      
      ;; evaluate each choice
      (until done?
	(multiple-value-setq (ch done?)
	  (funcall iter))
	(unless done?
	  
	  ;; evaluate this choice, and add it to the known-choices unless
	  ;; the q-value was unknown
	  (handler-case
	      (let ((val (evaluate q-fn omega ch)))
		(vector-push ch known-choices)
		(vector-push val vals))
	    (unknown-state-action ()))))
      
      (if (= (length vals) 0)
	  (error 'q-fn:unknown-state-action :state omega :action 'all-actions :q-fn q-fn)
	
	(multiple-value-bind (best val)
	    (argmax vals)
	  (values (aref known-choices best) val))))))



(defmethod boltzmann-dist ((q-fn <q-function>) omega temp)
  "boltzmann-dist Q-FN OMEGA CHOICES TEMP.  Return the boltzmann distribution (represented as a vector).  For now, a default value of 0 is used for unknown state-actions (e.g. when doing function approximation), which is probably a bad idea in general."
  (let ((choices (choices q-fn omega)))
    (prob:make-boltzmann-dist
     (mapset 'vector #'(lambda (choice) 
			 (handler-case 
			     (evaluate q-fn omega choice)
			   (unknown-state-action ()
			     (warn "Using default value 0 for unknown state-action in Boltzmann exploration.  This part of the code probably needs to be changed.")
			     0)))
	     choices)
     temp choices)))


(defmethod policy:print-advice ((adv q-fn:<q-function>) omega choices str)
  (handler-bind ((q-fn:unknown-state-action 
		  #'(lambda (c)
		      (declare (ignore c))
		      (use-value 'unknown))))
    (format str "~&Q-values ~a"
	    (let ((q-vals (make-array 0 :fill-pointer 0 :adjustable t)))
	    
	      (set:do-elements (u choices q-vals)
		(vector-push-extend (cons u 
					  (let ((val
						 (q-fn:evaluate adv omega u)))
					    (if (numberp val)
						(round-decimal val 2)
					      val)))
				    q-vals))))
    (handler-case (format str "~&Recommended choice is ~a" (q-fn:best-choice adv omega))
      (q-fn:unknown-state-action () (format str "~&Unable to determine best choice.")))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition unknown-state-action (error)
  ((state :initarg :state :reader state)
   (action :initarg :action :reader action)
   (q-fn :initarg :q-fn :reader q-fn))
  (:documentation "Signalled whenever a Q-function sees a state-action pair that it doesn't recognize.  The use-value restart may be provided, in case higher level code wants to provide a default value.")
  (:report (lambda (c s) 
	     (format s "Unknown state-action pair (~a . ~a) for q-function ~a." 
		     (state c) (action c) (q-fn c)))))

(define-condition unknown-state (error)
  ((state :initarg :state :reader state)
   (q-fn :initarg :q-fn :reader q-fn))
  (:documentation "Signalled whenever a Q-function sees a state that it doesn't recognize.")
  (:report (lambda (c s)
	     (format s "Unknown state ~a for q-function ~a."
		     (state c) (q-fn c)))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *num-decimal-places-to-print* 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restarts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun use-default-val-0 (c)
  (declare (ignore c))
  (use-value 0))

(defun choose-randomly (c)
  (declare (ignore c))
  (invoke-restart 'random-choice))

