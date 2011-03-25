(defpackage calisp-approximate-policy-iteration
  (:documentation "Package calisp-approximate-policy-iteration (capi).  Defines the <approx-pol-it> learning algorithm (not to be confused with the algorithm of the same name in the api package, which is for flat RL).

Approximate policy iteration is an on-policy SMDP Q-learning algorithm.  At periodic intervals, the Q-function is saved, and the exploration policy acts greedily with respect to the most recent saved Q-function.  The algorithm is closely related to 'optimistic policy iteration' and SARSA.

Exports
-------
<approx-pol-it>")
  (:export 
   <approx-pol-it>)
  (:use 
   calisp-obs
   cl
   utils)
  (:nicknames capi))

(in-package capi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <approx-pol-it> (<on-policy-learning-algorithm> <q-learning-algorithm> <calisp-learning-algorithm>)
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
   (prev-omega :accessor prev-omega)
   (prev-u :accessor prev-u)
   (reward-acc :accessor reward-acc :type float)
   (cumulative-discount :accessor cumulative-discount :type float))
  (:documentation "The <approx-pol-it> class.  Inherits from on-policy-learning-algorithm, q-learning-algorithm, and calisp-learning-algorithm.  Initargs

:lrate - <learning-rate>.  .01 by default.
:discount - 1 by default.
:debug-str - nil by default
:pol-imp-int - how often the policy is updated.  Required.
:q-fn - <q-function>

There are two additional arguments
:term-policy - a policy that is 'guaranteed' to terminate eventually.  By default, this is a random-policy
:pol-switch - if during learning, we have been in an episode for at least this many steps, then we switch to the guaranteed eventual termination policy.  It should be significantly higher than the amount of time taken by the termination-policy to terminate.  Not setting this variable amounts to setting it to infinity.

When asked for its knowledge state, a <approx-pol-it> object returns the currently estimated Q-function.  Its convert-to-policy method turns a Q-function into the corresponding greedy policy.
"))


(defmethod initialize-instance :after ((alg <approx-pol-it>) &rest args &key pol-imp-int)
  (declare (ignore args))
  (check-type pol-imp-int integer)
  (set-pol-imp-pred
   (lambda (x) (= (mod x pol-imp-int) 0))
   alg)

    (unless (slot-boundp alg 'term-policy)
    (set-term-policy 
     (make-instance 'policy:<random-policy> :choice-fn #'js-choices)
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
;; Rather than just implement the methods for 
;; <learning-algorithm>, we provide these operations as
;; separate functions.  This is convenient, because it allows
;; the code to be used by standard RL algorithms as well as
;; those for ALisp and concurrent ALisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-episode ((alg <approx-pol-it>) s)
  (declare (ignore s))
  (setf (prev-omega alg) nil
	(prev-u alg) nil
	(reward-acc alg) nil
	(cumulative-discount alg) nil))


(defmethod inform-env-step ((alg <approx-pol-it>) a r s2 term)
  (declare (ignore s2 a term))
  
  ;; update cumulative reward and discount
  (with-slots (reward-acc cumulative-discount discount q-function) alg
    (when reward-acc
      (incf reward-acc (* r cumulative-discount))
      (multf cumulative-discount discount))
  
    ;; if necessary, update the current Q-function being used for exploration
    (when (funcall (pol-imp-pred alg) (current-env-step alg))
      (set-exp-policy (make-instance 'policy:<greedy-policy> :q-function (clone q-function)) alg))))
  


(defmethod inform-calisp-step ((alg <approx-pol-it>) omega u)
  (with-slots (q-function prev-omega prev-u reward-acc 
	       cumulative-discount learning-rate)
      alg
    
    ;; do a backup if a previous state has been seen in this episode
    (when prev-omega
      (assert prev-u)
      (q-fn:update q-function prev-omega prev-u
		   (+ reward-acc
		      (* cumulative-discount
			 (handler-case
			     (q-fn:evaluate q-function omega u)
			   (q-fn:unknown-state-action () 0))))
		   (lrate:get-rate learning-rate (cons prev-omega prev-u))))
    
    ;; remember omega and u
    (setf prev-omega omega
	  prev-u u
	  reward-acc 0
	  cumulative-discount 1)))

(defmethod inform-part-prog-terminated ((alg <approx-pol-it>) omega)
  (declare (ignore omega))

  (with-slots (prev-omega prev-u learning-rate) alg
    ;; do a backup if necessary
    (when prev-omega
      (assert prev-u)
      (q-fn:update (q-fn alg) prev-omega prev-u
		   (reward-acc alg)
		   (lrate:get-rate learning-rate
				   (cons prev-omega prev-u))))
    (setf prev-omega nil
	  prev-u nil)))
    

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

   