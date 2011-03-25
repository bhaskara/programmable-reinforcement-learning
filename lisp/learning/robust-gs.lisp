(defpackage robust-gs
  (:documentation "
Package robust-gs .  Defines <robust-gs>, a subclass of <q-learning-alg> that implements the ``robust gold-standard'' algorithm that maintains rectangular confidence sets for the transition matrices based on samples (the reward function is assumed known) and performs robust dynamic programming on them to compute the Q-function.  Assumes states (resp actions) are integers between 0 and num-states - 1.

Operations inherited from <q-learning-alg>
- reset.   Reset the counts to be all 0
- observe.  Increment the appropriate count
- get-q-fn.  Compute q using robust dynamic programming and return it (as a NSxNA array).")
  
  (:use common-lisp
	q-learning-alg
	robust-dp)
  (:export reset
	   observe
	   get-q-fn
	   <robust-gs>))

(in-package robust-gs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <robust-gs> (<q-learning-alg>)
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
   (successors :type array
	       :initarg :successors
	       :initform nil
	       :reader successors)
   (term :type array
	 :initarg :term
	 :reader term)
   (delta :initarg :delta
	    :initform .1
	    :reader delta)
   (uncertainty-level :type float
		      :initarg :unc-level
		      :reader unc-level)
   )
  (:documentation "The <robust-gs> class.  Inherits from <q-learning-alg>.  Initargs
:num-states
:num-actions
:rew - reward matrix represented as NSxNA array
:discount - optional, default 1.0
:term - optional, default is no terminal states
:successors - 2d-array whose (s,a)th element is list of possible successor states of state-action pair (s,a), or a non-list to indicate all states are possible.  by default all successors are possible after any state-action pair.
:unc-level - uncertainty level used to construct confidence sets.  The lower this number, the more sure we want to be that our confidence set covers the true transition matrices, and so the more pessimistic we will be.
"))



(defmethod initialize-instance :after ((qa <robust-gs>) &rest args &aux (ns (num-states qa)))
  (unless (slot-boundp qa 'term)
    (setf (slot-value qa 'term) (make-array ns :element-type 'boolean :initial-element nil)))
  (setf (slot-value qa 'counts) (make-array (list (num-actions qa) ns ns)
					    :element-type 'fixnum
					    :initial-element 0)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((qa <robust-gs>))
  (loop
      with c = (counts qa)
      for i below (array-total-size c)
      do (setf (row-major-aref c i) 0)))


(defmethod observe ((qa <robust-gs>) s a r s2 &optional a2)
  (declare (ignore a2 r))
  (incf (aref (counts qa) a s s2)))


(defmethod get-q-fn ((qa <robust-gs>))
  (inf-horizon-rvi (counts qa) (rew qa) (discount qa) (unc-level qa) (term qa) (delta qa) :successors (successors qa)))




(in-package cl-user)

   