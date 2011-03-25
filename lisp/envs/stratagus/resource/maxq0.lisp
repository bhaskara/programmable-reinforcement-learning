(defpackage maxq0
  (:documentation
   "Package for the MAXQ0 algorithm and associated feature templates for concurrent ALisp programs.
") 
  (:use common-lisp
	utils
	policy
	calisp-obs)
  (:export
        <maxq0>))

(in-package maxq0)

(defclass <maxq0> (<calisp-learning-algorithm> <q-learning-algorithm>)
   ((prev-omega :accessor prev-omega)
    (prev-u :accessor prev-u)
    (prev-s :accessor prev-s)
    (initq :accessor initq)
    (time_elapsed :accessor time_elapsed :initform 0)
   (q-function :reader q-fn :type <maxq0-q-function> :initarg :q-fn :writer set-q-fn
	       :initform (make-instance 'maxq0-q-function:<maxq0-q-function>
			   :fn-approx (make-instance 'fn-approx:<linear-fn-approx>)))
   (eta-fn :reader eta-fn :initarg eta-fn :initform eta-linear-tabular)
   (reward-decomposer :reader reward-decomposer :initarg :reward-decomposer)
  (:documentation "Class <maxq0>.  Subclass of <calisp-learning-algorithm> that uses MAXQ0-learning.

Initargs
:t-lrate learning rate
:q-fn q-function
"))

(defmethod initialize-instance :after ((alg <maxq>) &rest args &key q-function)
  (declare (ignore args))
  (setf (initq alg) (clone q-function)))

(defmethod inform-start-episode ((alg <maxq0>) s)
  (setf (prev-s alg) s)
  (slot-makunbound alg 'prev-omega)
  (slot-makunbound alg 'prev-u))

(defmethod inform-calisp-step ((alg <maxq0>) omega u)
  (setf (prev-omega alg) omega
	(prev-u alg) u))

(defmethod inform-env-step ((alg <maxq0) act rew next term)
  (when (slot-boundp alg 'prev-omega)
    (let ((rewards (funcall (reward-decomposer alg) (prev-omega alg) (prev-s alg) act rew next)))
      (assert (= rew (reduce #'+ rewards :key #'cdr)) ()
        "Rewards ~a don't add up to ~a in reward decomposition for doing ~a in ~a and getting to ~a" rewards rew act (prev-s alg) next)
       (dolist (trew rewards)
	 (let ((tid (car trew))
	       (drew (cdr trew)))
	   (update-tid (q-fn alg) (prev-omega alg) nil drew 
	     (funcall (eta-fn alg) (get-subtask (prev-omega alg) tid) time_elapsed) tid)))))
  (setf (prev-s alg) next))
      

(defmethod inform-end-choice-block ((alg <maxq0>) tid omega)
  (update-tid (q-fn alg) (prev-omega alg) (get-subtask omega tid) 
      (evaluate-max-node omega (get-subtask (prev-omega alg) tid) tid)
      (funcall (eta-fn alg) (get-subtask (prev-omega alg) tid) time_elapsed) tid))
	  
(defun eta-linear-tabular (subtask time)
  (if (equalp subtask "goto") 0.001
    0.01))

;; NOTE: Ask Bhaskara whether there is an easy way to store the previous omega with respect to 
;; inform-end-choice-block