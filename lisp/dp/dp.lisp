(defpackage dynamic-programming
  (:documentation "contains various dynamic programming algorithms for markov chains, mdps, and smdps.

- exit-dist
- value-iteration 
- policy-iteration 
- policy-evaluation
- q-from-v
- greedy policy

the algorithms explicitly form the transition and reward matrices, and will therefore probably die on large mdps
")
  (:nicknames dp)
  (:use common-lisp
	utils
	mdp
	lin-alg
	prob
	set)
  (:export 
   exit-dist
   value-iteration
   policy-iteration
   policy-evaluation
   q-from-v
   greedy-policy))

(in-package dynamic-programming)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric value-iteration (m &key (epsilon .01) (verbose nil) (init-val) (discount 1.0))
  (:documentation
   "value-iteration M &key (EPSILON .01) (VERBOSE NIL) (INIT-VAL) (DISCOUNT 1.0).  Perform value iteration on M, until value function doesn't change by more than EPSILON.  Starts with INIT-VAL, or the 0 vector if it's not provided.  Updates are done serially in that if a state s1 is updated during an iteration, and state s2 is later updated during the same iteration, then the update for s2 makes use of the new value of s1.

Methods are currently provided for M being an <mdp> or <sparse-tabular-smdp>.  

Returns value function stored in a vector."))

(defgeneric policy-iteration (m &key (epsilon -1) (k -1) (verbose nil) (init-pol) (discount 1.0))
  (:documentation "policy-iteration M &key (EPSILON -1) (K -1) (VERBOSE nil) (INIT-POL)  (DISCOUNT 1.0).  Performs (modified) policy iteration on M.  Starts with INIT-POL or policy of all 0's if it's not provided.  Each policy improvement step is exact and the policy evaluation step either value iteration using stopping parameters epsilon and k (at least one of which must be given a positive value).  Terminates when policy is unchanged.

Methods are currently provided for M being an <mdp> or <sparse-tabular-smdp>.n

 Returns two values.  1) policy stored in an array 2) value function of the policy.

 Note : if epsilon version is used, and the initial policy is improper, then the first policy evaluation step will diverge."))

(defgeneric policy-evaluation (m pol &key (epsilon .01) (discount 1.0))
  (:documentation "policy-evaluation M POL &key (EPSILON .01) (DISCOUNT 1.0).  Perform policy evaluation on policy POL in mdp M and return its value function.  Methods are currently provided for M being an <mdp> or <sparse-tabular-smdp>."))

(defgeneric q-from-v (m v &optional discount)
  (:documentation "q-from-v M V &optional (DISCOUNT 1.0).  Return the Q-function corresponding to value function V in M."))

(defun greedy-policy (m v &optional (discount 1.0))
  "greedy-policy M V &optional (DISCOUNT 1.0).  Return the greedy policy corresponding to value function V in M."
  (make-instance 'policy:<greedy-policy> :q-function (q-from-v m v discount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; inner function for policy evaluation that writes value function onto array v
(defun policy-evaluation-inner (trans rew pol v discount &key (epsilon -1) (k -1) (verbose nil))
   (loop 
       with num-states = (length v)
       with (delta new-val) 
       for i from 0
	       
       do (setf delta
 	   (loop
 	       for i below num-states
 	       do (setf new-val (backup v i (aref pol i) trans rew discount))
 	       maximizing (abs (- new-val (aref v i)))
 	       do (setf (aref v i) new-val)))
	 
	  (when verbose (format t "~&~a" delta))
       while (> delta epsilon)
       while (or (< k 0) (< i k))
	    
       finally (return v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions for creating tabular reps.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

(defun val-fn-to-vector (val-fn states)
  (let ((v (make-array (size states) :element-type 'float)))
    (do-elements (s states v i)
      (setf (aref v i) (value-fn:value val-fn s)))))

(defun pol-to-vector (pol states)
  (let ((p (make-array (size states) :element-type 'fixnum)))
    (do-elements (s states p i)
      (setf (aref p i)
	(policy:make-choice pol s) ))))



(in-package common-lisp-user)    


