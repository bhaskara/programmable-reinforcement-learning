(defpackage q-learning-alg
  (:documentation "
Package q-learning-alg (aka ql-alg).  Defines <q-learning-alg>, an abstract class representing any object that maintains and updates an estimate of a Q-function given a stream of samples.  Subclasses implement algorithms like Q-learning and prioritized sweeping.

Subclasses must implement
- reset
- observe
- get-q-fn
")
  (:nicknames ql-alg)
  (:use common-lisp)
  (:export <q-learning-alg>
	   reset
	   observe
	   get-q-fn))

(in-package q-learning-alg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
	   
(defclass <q-learning-alg> ()
  ()
  (:documentation "Class <q-learning-alg> (abstract)."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations to be implemented by subclasses 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reset (qa)
  (:documentation "reset QA.  Forget all that has been learnt so far and reinitialize parameters."))

(defgeneric observe (qa s a r s2 &optional a2)
  (:documentation "observe QA S A R S2 &optional A2.  Observe a sampled transition from S to S2 under action A yielding reward R.  A2 is the action done at the next state.  A2 is ignored in many implementations (e.g. Q-learning), but must be provided for some (e.g. SARSA)."))

(defgeneric get-q-fn (qa)
  (:documentation "get-q-fn QA.  Return a fresh copy of the parameters of the current Q-function."))





(in-package cl-user)
