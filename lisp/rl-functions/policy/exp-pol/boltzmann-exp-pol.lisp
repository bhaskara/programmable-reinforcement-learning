(in-package exploration-policy)

(defclass <boltzmann-exp-pol> (<exp-pol>)
  ((alg :initarg :q-learning-alg :reader alg :type rl-obs:<q-learning-algorithm>)
   (temperature-function :initarg :temp-fn :reader temp-fn))
  (:documentation "A subclass of <exp-pol> for Boltzmann exploration based on a Q-function.  

Required initargs
:q-learning-alg - Any object of type <q-learning-algorithm>.  The current Q-estimate of the algorithm is used to make decisions.
:temp-fn - Temperature function, that takes in a count >=0 and returns a temperature, which is either a nonnegative real number or the symbol 'utils:infty.  Temperatures near 0 imply almost-greedy policies, and temperatures near infinity imply almost uniformly random policies."))

(defmethod get-choice-dist ((pol <boltzmann-exp-pol>) omega count)
  (let ((alg (alg pol)))
    (q-fn:boltzmann-dist 
     (rl-obs:get-q-fn alg (rl-obs:knowledge-state alg nil))
     omega (funcall (temp-fn pol) count))))


(defvar *temps* (make-array 0 :adjustable t :fill-pointer 0))

(defun make-temp-decay-fn (g1 c g2 &optional (diff 1))
  "make-temp-decay-fn G1 C G2 &optional (DIFF 1).
A simple way to create temperature decay functions for Boltzmann exploration policies.  Consider a Boltzmann distribution that chooses u with probability proportional to e^(Q(u)/T).  Now consider the quantity G = e^(DIFF/T).  G stands for greediness - given an action u_1 whose Q-value is DIFF more than u_2, u_1 will be G times more likely under this Boltzmann distribution.  

make-temp-decay-fn returns temperature decay function (that takes in a count and returns a temperature), such that the greediness when the count=0 is G1, and the greediness when count=C is G2.  The temperature is interpolated by assuming that greediness is an affine function of count.

Note that, in the cases where there's a huge action space, a high greediness value might be needed if you want to ensure that the optimal action gets done."
  
  (let ((q (/ (- g2 g1) c)))
    (lambda (count)
      (let ((temp (/ diff (log (+ g1 (* count q))))))
	(vector-push-extend temp *temps*)
	temp))))

