(defpackage policy
  (:documentation "Package policy (pol).  

Types
-----
[policy]
<policy>
<stochastic-policy>
<greedy-policy>
<random-policy>
<tabular-policy>
<prompt-policy>

Methods to be implemented by subclasses of <policy>
---------------------------------------------------
make-choice

Methods to be implemented by subclasses of <stochastic-policy>
--------------------------------------------------------------
choice-dist


Creating policies
-----------------
make-random-policy
make-tabular-policy
policy-case

Customizing behaviour of <prompt-policy>
----------------------------------------
print-advice
prompt-for-choice

Conditions and restarts
-----------------------
choose-to-abort
unknown-state-or-action
unknown-state
")
  (:use
   common-lisp
   prob
   utils)
   
  (:nicknames pol)
  (:export
   [policy]
   <policy>
   <greedy-policy>
   <random-policy>
   <prompt-policy>
   <tabular-policy>
   <stochastic-policy>
   
   make-random-policy
   make-tabular-policy
   policy-case
   
   make-choice
   choice-dist
   
   print-advice
   prompt-for-choice
   
   choose-to-abort
   unknown-state
   unknown-state-action
   ))
  

(in-package policy)


(deftype [policy] ()
  "Type for policies.  Can either be an object of type <policy>, or a function."
  `(or <policy> function))

(defclass <policy> ()
  ()
  (:documentation "Class for representing policies.  A policy is any object that is used to make decisions as a function of some state.  This can include random-choices, making greedy choices w.r.t a q-function, or even 'policies' that prompt the user to enter a choice."))

(defclass <stochastic-policy> (<policy>)
  ()
  (:documentation "Class <stochastic-policy> (<policy>).  Implements a method for choice-dist, which returns a probability distribution over choices.  The make-choice method for stochastic policies just samples from this distribution."))

(defgeneric make-choice (d omega)
  (:documentation "make-choice POLICY STATE.  Specifies how this policy makes choices at a given state.  An 'unknown-state error might be thrown.")
  (:method ((d <stochastic-policy>) omega)
	   (sample (choice-dist d omega)))
  (:method ((d function) omega)
	   (funcall d omega)))

(define-condition choose-to-abort (serious-condition)
  ()
  (:documentation "Signalled (using error) by a policy's make-choice method when, for some reason, rather than making a choice, it has decided to 'abort'.  For example, the prompt-policy would signal this if the user decided to quit rather than entering a choice."))

(defgeneric choice-dist (pol omega)
  (:documentation "choice-dist STOCHASTIC-POLICY OMEGA.  Return an object of type [prob-dist] over the choices at this state."))


(defmacro policy-case (fn &rest l)
  "Macro policy-case FN (V1 P1) ... (Vn Pn)

FN - a function (evaluated)
Vi - a symbol or number (not evaluated)
Pi - a policy (evaluated)

Return a policy that makes choices by applying FN to the state OMEGA, then finds one of the Vi such that the returned value of FN is #'eql to Vi, and makes a choice using the corresponding policy Pi.  There must always be exactly one Vi that matches, otherwise it is an error."
  
  (with-gensyms (func omega)
    (let ((pol-gensyms (loop for i below (length l) collecting (gensym))))
      `(let ((,func ,fn)
	     ,@(mapcar #'(lambda (s p) (list s (second p))) pol-gensyms l))
	 #'(lambda (,omega)
	     (make-choice
	      (ecase (funcall ,func ,omega)
		,@(mapcar #'(lambda (s p) (list 
					   (let ((fp (first p)))
					     (if (member fp '(t otherwise nil))
						 (list fp)
					       fp))
					   s))
			  pol-gensyms l))
	      ,omega))))))
	 

