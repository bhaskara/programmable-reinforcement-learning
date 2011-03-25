(defpackage max-likelihood-rl-gs
  (:documentation "
Package max-likelihood-rl-gs (mle-rl-gs).  Defines <mle-gs>, a subclass of <q-learning-alg> that implements the ``gold-standard'' algorithm that maintains a maximum-likelihood estimate of the transition matrices (the reward function is assumed known) and performs dynamic programming on them to compute the Q-function.  Assumes states (resp actions) are integers between 0 and num-states - 1.

Operations inherited from <q-learning-alg>
- reset.   Reset the counts to be all 0
- observe.  Increment the appropriate count
- get-q-fn.  Compute q using modified policy iteration and return it (as a NSxNA array).")
  
  (:nicknames mle-rl-gs)
  (:use common-lisp
	q-learning-alg
	tabular-mdp
	dp)
  (:export reset
	   observe
	   get-q-fn
	   <mle-gs>))

(in-package max-likelihood-rl-gs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <mle-gs> (<q-learning-alg>)
  ((reward-matrix :initarg :rew
		  :type array
		  :reader rew)
   (num-states :initarg :num-states
	       :reader num-states
	       :type fixnum)
   (num-actions :type fixnum
		:initarg :num-actions
		:reader num-actions)
   (discount :type float
	     :initarg :discount
	     :initform 1.0
	     :reader discount)
   (counts :type array
	   :reader counts)
   (pol :type array
	:reader pol
	:writer set-pol)
   (successors :type array
	       :initarg :successors
	       :reader successors
	       :initform nil)
   (mdp :type <tabular-mdp>
	:reader mdp)
   (term :type array
	 :initarg :term
	 :reader term)
   )
  (:documentation "The <mle-gs> class.  Inherits from <q-learning-alg>.  Initargs
:num-states
:num-actions
:rew (reward matrix represented as NSxNA array)
:discount (optional, default 1.0)
:successors - an array whose (s,a)th element is the list of states that are possible successors of state-action pair (s,a) or a non-list to indicate that all states are possible successors.  When there is no data for a state-action pair, a uniform distribution over the successors is used.  Optional, and by default all states are possible successors to all state-action pairs.
:term (optional, default is no terminal states)"))


(defmethod initialize-instance :after ((qa <mle-gs>) &rest args &aux (ns (num-states qa)))
  (unless (slot-boundp qa 'term)
    (setf (slot-value qa 'term) (make-array ns :element-type 'boolean :initial-element nil)))
  (setf (slot-value qa 'counts) (make-array (list ns (num-actions qa) ns)
					    :element-type 'fixnum
					    :initial-element 0))
  (set-pol (make-array ns :element-type 'fixnum :initial-element 0) qa)
  (setf (slot-value qa 'mdp) 
    (make-instance '<tabular-mdp> :rew (rew qa) :discount (discount qa)
		   :term (term qa) :trans (make-array (list ns (num-actions qa) ns)
							    :initial-element 0))))



(defconstant *mpi-k* 20)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((qa <mle-gs>))
  (loop
      with c = (counts qa)
      for i below (array-total-size c)
      do (setf (row-major-aref c i) 0)))


(defmethod observe ((qa <mle-gs>) s a r s2 &optional a2)
  (declare (ignore a2 r))
  (incf (aref (counts qa) s a s2)))


(defmethod get-q-fn ((qa <mle-gs>) &aux (m (mdp qa)))
  (set-trans (get-mle-trans qa (counts qa)) m)
  (multiple-value-bind (pol val)
      (dp:policy-iteration m :k *mpi-k* :init-pol (pol qa))
    (set-pol pol qa)
    (q-from-v m val)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-mle-trans (qa counts)
  (loop
      with ns = (num-states qa)
      with na = (num-actions qa)
      with succ = (successors qa)
      with term = (term qa)
      with trans = (make-array (list ns na ns) :initial-element 0)
		   
      for i below ns
		  
      unless (aref term i)
      do (loop
	     for a below na 
	     for total = (loop for j below ns summing (aref counts i a j))
			 
	     do (if (> total 0)
		    (loop for j below ns do (setf (aref trans i a j) (/ (aref counts i a j) total)))
		  (if succ
		      (loop 
			  with prob = (/ 1 (length (aref succ i a)))
			  for j in (aref succ i a)
				   
			  do (setf (aref trans i a j) prob))
					
		    (loop for j below ns do (setf (aref trans i a j) (/ 1 ns))))))
	 
      finally (return trans)))
		  



(in-package cl-user)

   