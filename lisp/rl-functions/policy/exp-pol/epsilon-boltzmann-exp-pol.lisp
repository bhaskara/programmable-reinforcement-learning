(in-package exploration-policy)

(defclass <epsilon-boltzmann-exp-pol> (<boltzmann-exp-pol>)
  ((epsilon-decay-fn :type function :reader epsilon-decay-fn :initarg :epsilon-decay-fn))
  (:documentation "A subclass of <boltzmann-exp-pol> that combines it with epsilon-greedy exploration.

Required initargs

:q-learning-alg - Any object of type <q-learning-algorithm>.  The current Q-estimate of the algorithm is used to make decisions.
:epsilon-decay-fn - Epsilon function that takes in a count >= 0 and returns epsilon between 0 and 1.  At each step, with probability epsilon, the boltzmann policy is used, and otherwise the greedy action is selected.
:temp-fn - Temperature function, that takes in a count >=0 and returns a temperature, which is either a nonnegative real number or the symbol 'utils:infty.  Temperatures near 0 imply almost-greedy policies, and temperatures near infinity imply almost uniformly random policies."))

(defmethod get-choice-dist ((pol <epsilon-boltzmann-exp-pol>) omega count)
  (let ((alg (alg pol))
	(eps (funcall (epsilon-decay-fn pol) count))
	(r (random 1.0)))
    (q-fn:boltzmann-dist 
     (rl-obs:get-q-fn alg (rl-obs:knowledge-state alg nil))
     omega 
     (if (< r eps)
	 (funcall (temp-fn pol) count)
       0))))

(defun make-linear-epsilon-decay-fn (c e)
  "make-linear-epsilon-decay-fn C E.

Returns a function of one argument T, that equals 1 when T is 0, and decreases linearly, until it equals E when T=C.  After that, it stays constant at E."
  (lambda (s)
    (cond ((= e 1) 1)
	  ((> s c) e)
	  (t (+ e
		(/ (* (- c s) (- 1 e))
		   c))))))