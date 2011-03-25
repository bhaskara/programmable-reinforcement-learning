(defpackage approximate-policy-iteration
  (:documentation "Package approximate-policy-iteration (api).  Defines the <approx-pol-it> learning algorithm.  

Approximate policy iteration is an on-policy Q-learning algorithm.  At periodic intervals, the Q-function is saved, and the exploration policy acts greedily with respect to the most recent saved Q-function.  The algorithm is closely related to 'optimistic policy iteration' and SARSA.

Exports
-------
<approx-pol-it>")
  (:export 
   <approx-pol-it>)
  (:use 
   rl-obs
   cl
   utils)
  (:nicknames api))

(in-package api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <approx-pol-it> (<on-policy-learning-algorithm> <q-learning-algorithm>)
  ((discount :type float
	     :initarg :discount
	     :initform 1.0
	     :reader discount)
   (learning-rate :type lrate:<learning-rate>
		  :initarg :lrate
		  :initform .01
		  :reader lrate)
   (q-function :type q-fn:<q-function>
	       :reader q-fn
	       :initarg :q-fn
	       :writer set-q-fn)
   (init-q-function :type q-fn:<q-function>
		    :reader init-q-fn
		    :writer set-init-q-fn)
   (policy-improvement-pred :type function 
			    :reader pol-imp-pred
			    :writer set-pol-imp-pred)
   (current-exploration-policy :type policy:[policy]
			       :reader exp-policy
			       :writer set-exp-policy)
   (term-policy :type policy:[policy]
		:reader term-policy
		:writer set-term-policy
		:initarg :term-policy)
   (term-policy-switch-threshold :type fixnum
				 :reader pol-switch
				 :initform nil
				 :initarg :pol-switch)
   (prev-s :accessor prev-s)
   (prev-a :accessor prev-a)
   (prev-r :accessor prev-r)
   (prev-s2 :accessor prev-s2))
  (:documentation "The <approx-pol-it> class.  Inherits from <learning-algorithm>.  Initargs

:lrate - <learning-rate>.  .01 by default.
:discount - 1 by default.
:debug-str - nil by default
:pol-imp-int - how often the policy is updated.  Required.
:q-fn - <q-function> - either this or env must be provided.
:env - the environment (if q-fn is not provided, defaults to a tabular q-function for this environment)

There are two additional arguments
:term-policy - a policy that is 'guaranteed' to terminate eventually.  By default, this is a random-policy
:pol-switch - if during learning, we have been in an episode for at least this many steps, then we switch to the guaranteed eventual termination policy.  It should be significantly higher than the amount of time taken by the termination-policy to terminate.  Not setting this variable amounts to setting it to infinity.

When asked for its knowledge state, a <approx-pol-it> object returns the currently estimated Q-function.  Its convert-to-policy method turns a Q-function into the corresponding greedy policy.
"))


(defmethod initialize-instance :after ((alg <approx-pol-it>) &rest args &key pol-imp-int env)
  (declare (ignore args))
  (check-type pol-imp-int integer)
  (set-pol-imp-pred
   (lambda (x) (= (mod x pol-imp-int) 0))
   alg)
  (unless (slot-boundp alg 'q-function)
    (set-q-fn (make-instance 'q-fn:<env-q-function> :env env) alg))
  
  (unless (slot-boundp alg 'term-policy)
    (set-term-policy 
     (make-instance 'policy:<random-policy> :env env)
     alg))
  
  (let ((cloned-q (clone (q-fn alg))))
    (set-init-q-fn cloned-q alg)
    (set-exp-policy 
     (make-instance 'policy:<greedy-policy> :q-function cloned-q)
     alg)
    ;; So init-q and exp-q are the same object to begin with
    ;; but this shouldn't cause problems because neither will ever
    ;; be modified
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-episode ((alg <approx-pol-it>) s)
  (setf (prev-s2 alg) s
	(prev-s alg) nil
	(prev-a alg) nil
	(prev-r alg) nil))
  

(defmethod inform-env-step ((alg <approx-pol-it>) a r s2 term)
  (with-slots (q-function prev-s prev-a prev-r prev-s2 learning-rate) alg

    ;; at this point, we know that, assuming prev-s is non-nil,
    ;; prev-s -> prev-a -> prev-r -> prev-s2 -> a -> r -> s2

    (let ((eta (lrate:get-rate learning-rate (cons prev-s a))))

      ;; first do a SARSA backup to prev-s,prev-a if possible
      (when prev-s
	(assert (and prev-a prev-r))
	(q-fn:update q-function prev-s prev-a
		     (+ prev-r 
			(* (discount alg)
			   (handler-case
			       (q-fn:evaluate q-function prev-s2 a)
			     (q-fn:unknown-state-action () 0))))
		     eta))
    
      ;; also if s2 is terminal, do a backup to prev-s2, a
      (if term
	  (progn
	    (q-fn:update q-function prev-s2 a r eta)
	    (setf prev-s nil
		  prev-a nil
		  prev-r nil
		  prev-s2 nil))
      
	;; otherwise, just update prev-values
	(setf prev-s prev-s2
	      prev-a a
	      prev-r r
	      prev-s2 s2)))
    
    ;; if necessary, update the current Q-function being used for exploration
    (when (funcall (pol-imp-pred alg) (current-env-step alg))
      (set-exp-policy (make-instance 'policy:<greedy-policy> :q-function (clone q-function)) alg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations from <on-policy-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-choice ((alg <approx-pol-it>) omega)
  (make-choice
   (if (aand (pol-switch alg) (> (current-episode-step alg) it))
       (term-policy alg)
     (exp-policy alg))
   omega))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <q-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <approx-pol-it>))
  (set-q-fn (clone (init-q-fn alg)) alg)
  (lrate:reset-counts (lrate alg)))

(defmethod knowledge-state ((alg <approx-pol-it>) &optional (fresh t))
  "The state of knowledge is just the Q-function."
  (let ((q (q-fn alg)))
    (if fresh (clone q) q)))

(defmethod get-q-fn ((alg <approx-pol-it>) ks)
  ks)

		  



(in-package cl-user)

   