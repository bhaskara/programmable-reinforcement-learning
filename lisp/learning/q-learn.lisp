(defpackage q-learn
  (:documentation "
Package q-learn.  Defines <q-learn>, a subclass of <q-learning-alg> that implements the standard Q-Learning algorithm.  Assumes states (resp actions) are integers between 0 and num-states - 1.

Operations inherited from <q-learning-alg>
- reset.   Reset the counts to be all 0
- observe.  Increment the appropriate count
- get-q-fn.  Compute q using modified policy iteration and return it (as a NSxNA array).")
  
  (:use common-lisp
	learning-rate
	q-learning-alg)
  (:export reset
	   observe
	   get-q-fn
	   <q-learn>))

(in-package q-learn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <q-learn> (<q-learning-alg>)
  ((num-actions :type fixnum
		:initarg :num-actions
		:reader num-actions)
   (discount :type float
	     :initarg :discount
	     :initform 1.0
	     :reader discount)
   (learning-rate :type <learning-rate>
		  :initarg :l-rate
		  :reader l-rate)
   (q-fn :type fn-approx:<fn-approx>
	 :initarg :fn-approx
	 :reader q-fn)
   )
  (:documentation "The <q-learn> class.  Inherits from <q-learning-alg>.  Initargs
:num-actions
:learning-rate - Object of type <learning-rate>
:fn-approx - Object of type <fn-approx>
:discount (optional, default 1.0)"))



(defmethod initialize-instance :after ((qa <q-learn>) &rest args)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation of operations inherited from <q-learning-alg>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((qa <q-learn>))
  (fn-approx:reset (q-fn qa)))


(defmethod observe ((qa <q-learn>) s a r s2 &optional a2 &aux (q (q-fn qa)))
  (declare (ignore a2))
  
  (fn-approx:update q (list s a) 
		    (+ r 
		       (loop for a-next below (num-actions qa) 
			   maximizing (fn-approx:evaluate q (list s2 a-next))))
		    (learning-rate:get-rate (l-rate qa) (list s a))))


(defmethod get-q-fn ((qa <q-learn>))
  (fn-approx:get-params (q-fn qa)))

		  



(in-package cl-user)

   