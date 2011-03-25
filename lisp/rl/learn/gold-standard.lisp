(defpackage gold-standard
  (:documentation "Defines the <gold-standard> reinforcement learning algorithm.

Exports
-------
<gold-standard>
make-gold-standard-learning-algorithm")
  (:export
   <gold-standard>
   make-gold-standard-learning-algorithm)
  (:use 
   cl
   rl-obs
   utils))

(in-package gold-standard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <gold-standard> (<q-learning-algorithm>)
  ((discount :type float
	     :initarg :discount
	     :initform 1.0
	     :reader discount)
   (states :type set:[numbered-set]
	   :accessor states)
   (actions :type set:[numbered-set]
	    :accessor actions)
   (transition-counts :type (simple-array fixnum 3)
		      :accessor trans)
   (reward-matrix :type (simple-array float 3)
		  :accessor reward)
   (transition-matrix :type (simple-array float 3)
		      :accessor trans-mat)
   (term-vector :type (simple-array boolean 1)
		:accessor term)
   (state-action-counts :type (simple-array fixnum 2)
			:accessor sa-counts)
   (previous-state :accessor prev-s))
  (:documentation "Implements 'gold-standard' model-based reinforcement-learning, i.e., the algorithm that maintains maximum likelihood estimates of the transition distribution and reward matrix, and does DP on these on each step (inefficient). Initargs are
:discount.

Gold standard learning works as follows.  We assume rewards are deterministic functions of s,a,s2, and so just keep track of the last observed reward for each triple.  We also keep counts of how often each s,a,s2 has been observed to estimate transition probabilities.  We use a standard multinomial estimate for transition probabilities, except in the case when there are no observations for a given s,a, in which case we assume that doing a in s leads back to s with probability 1, and reward 0.  Gold standard learning is a subclass of <q-learning-algorithm>, and when asked for its current q-function estimate, it does dynamic programming based on its current estimates of T and R (as opposed to, say doing it online as the samples come in like prioritized sweeping does).  "))



(defconstant *mpi-k* 8)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod shared-initialize :after ((alg <gold-standard>) names &rest args)
  (declare (ignore names args))
  (setf (states alg) (make-array 0 :adjustable t)
	(actions alg) (make-array 0 :adjustable t)
	(term alg) (make-array 0 :adjustable t)
	(trans alg) (make-array '(0 0 0) :element-type 'fixnum)
	(trans-mat alg) (make-array '(0 0 0) :element-type 'float)
	(reward alg) (make-array '(0 0 0) :element-type 'float)
	(sa-counts alg) (make-array '(0 0) :element-type 'fixnum)))


(defun make-gold-standard-learning-algorithm (&key (discount 1.0) (debug-str nil))
  "make-gold-standard-learning-algorithm &key (DISCOUNT 1.0)"
  (make-instance '<gold-standard> :discount discount :debug-str debug-str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro num-states (alg)
  `(set:size (states ,alg)))

(defmacro num-actions (alg)
  `(set:size (actions ,alg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <rl-observer>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-episode ((alg <gold-standard>) s)
  (notice-state alg s nil)
  (setf (prev-s alg) s))

(defmethod inform-env-step ((alg <gold-standard>) a r s2 term)
  (notice-state alg s2 term)
  (notice-action alg a)
  
  
  
  (let* ((i (set:item-number (prev-s alg) (states alg)))
	 (j (set:item-number a (actions alg)))
	 (k (set:item-number s2 (states alg))))
    
    ;; rewards are assumed not random, so just use r
    (setf (aref (reward alg) i j k) r)
    
    (incf (aref (sa-counts alg) i j))
    (incf (aref (trans alg) i j k))
    )
  
  (setf (prev-s alg) s2))


	  
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations from <q-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <gold-standard>))
  (reinitialize-instance alg))

(defmethod knowledge-state ((alg <gold-standard>) &optional (fresh t))
  "When asked for state of knowledge, the algorithm computes the current MDP, then does dynamic programming and returns the MDP and the Q-function in a list."
  
  (let ((m (mdp:make-tabular-mdp (update-transition-matrix alg)
				 (reward alg)
				 :state-set (states alg)
				 :action-set (actions alg)
				 :termination-vector (term alg)
				 :fresh fresh)))
    
    

    
    (multiple-value-bind (pol val)
	(dp:policy-iteration m :k *mpi-k* :discount (discount alg))
      (declare (ignore pol))
      (list m (dp:q-from-v m val (discount alg))))))



(defmethod get-q-fn ((alg <gold-standard>) ks)
  (second ks))

(defmethod get-mdp ((alg <gold-standard>) ks)
  (first ks))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-transition-matrix (alg)
  "recompute the transition matrix to reflect the current counts.  Assume it is already the correct size.  Return the updated transition matrix (which is also stored with the mdp)."
  (let ((sa-counts (sa-counts alg))
	(trans-counts (trans alg))
	(trans-mat (trans-mat alg))
	(num-states (num-states alg))
	(num-actions (num-actions alg)))
	
    (dotimes (i num-states trans-mat)
      (dotimes (j num-actions)
	(let ((sa-count (aref sa-counts i j)))
	  (if (= 0 sa-count)
	      (progn
		(dotimes (k num-states)
		  (setf (aref trans-mat i j k) 0.0))
		(setf (aref trans-mat i j i) 1.0))
	    (dotimes (k num-states)
	      (setf (aref trans-mat i j k)
		(/ (aref trans-counts i j k) sa-count)))))))))

  


(defun notice-state (alg s term)
  (unless (set:member? s (states alg))
    (set:addf (states alg) s)
    (let ((n (num-states alg))
	  (m (num-actions alg)))
      (adjustf (term alg) n :initial-element term)
      (adjustf (trans alg) (list n m n) :initial-element 0)
      (adjustf (trans-mat alg) (list n m n) :initial-element -42.0)
      (adjustf (reward alg) (list n m n) :initial-element 0.0)
      (adjustf (sa-counts alg) (list n m) :initial-element 0))
    
    ))

(defun notice-action (alg a)
  (unless (set:member? a (actions alg))
    (set:addf (actions alg) a)
    (let ((m (num-actions alg))
	  (n (num-states alg)))
      (adjustf (trans alg) (list n m n) :initial-element 0)
      (adjustf (trans-mat alg) (list n m n) :initial-element -42.0)
      (adjustf (reward alg) (list n m n) :initial-element 0.0)
      (adjustf (sa-counts alg) (list n m) :initial-element 0))
    
    ))

      
    


(in-package cl-user)

   