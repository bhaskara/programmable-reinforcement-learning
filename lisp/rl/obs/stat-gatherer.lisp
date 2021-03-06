(defpackage stat-gatherer
  (:documentation "Package for the <stat-gatherer> subclass of <rl-observer>.

Exports
-------
<stat-gatherer>
make-stat-gatherer
total-reward
num-episodes-started
num-episodes-finished
num-env-steps")
  (:use common-lisp
	rl-obs
	utils)
  (:export
   <stat-gatherer>
   make-stat-gatherer
   total-reward
   num-episodes-started
   num-episodes-finished
   num-env-steps))

(in-package stat-gatherer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <stat-gatherer> (<rl-observer>)
  ((total-reward 
    :accessor rew 
    :reader total-reward 
    :initform 0)
   (num-episodes-started 
    :accessor num-ep-started 
    :reader num-episodes-started 
    :initform 0)
   (num-episodes-finished 
    :accessor num-ep-finished 
    :reader num-episodes-finished
    :initform 0)
   (num-env-steps 
    :accessor sg-num-env-steps 
    :reader num-env-steps 
    :initform 0)
   (reset-upon-execution 
    :accessor reset? 
    :initarg :reset? 
    :initform t))
  
  (:documentation "Class for gathering stats about a run of reinforcement-learning.  Resets all counts the run function is called by default.  Right now counts total reward, num episodes started, num episodes ended, num env steps, num choice states.  Can add more stats in future, but make sure everything is O(1) space (so we can include this class as a default observer and not worry about memory).

Initargs
:reset? - whether to reset each time run is called.


"))


(defun make-stat-gatherer (&key (reset? t))
  "make-stat-gatherer &key (RESET-EVERY-TIME? t).  Return a new <stat-gatherer> object."
  (make-instance '<stat-gatherer> :reset? reset?))

(defmethod print-object ((obs <stat-gatherer>) str)
  (format str "~&RL stats~&~a episodes started and ~a finished~&~a env steps"
	  (num-episodes-started obs) (num-episodes-finished obs) (num-env-steps obs) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods overridden from <rl-observer>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-execution ((obs <stat-gatherer>))
  (when (reset? obs)
    (setf (num-ep-started obs) 0
	  (num-ep-finished obs) 0
	  (sg-num-env-steps obs) 0
	  (rew obs) 0)))

(defmethod inform-start-episode ((obs <stat-gatherer>) s)
  (declare (ignore s))
  (incf (num-ep-started obs)))



(defmethod inform-env-step ((obs <stat-gatherer>) act rew to term)
  (declare (ignore to act))
  (incf (rew obs) rew)
  (incf (sg-num-env-steps obs))
  (when term (incf (num-ep-finished obs))))

