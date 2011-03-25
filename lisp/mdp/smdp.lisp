(in-package mdp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outcomes of transitions in smdp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note - don't make outcome a true struct as other code may rely
;; on being able to compare outcomes with equalp
(defstruct (outcome (:type vector) (:constructor make-outcome (state reward duration)))
  state
  reward
  duration)

(defun outcome-p (x)
  (and (vectorp x)
       (= (length x) 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smdp class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <smdp> ()
  ((state-set :initarg :state-set
	      :writer set-state-set
	      :type [numbered-set]
	      :reader state-set)
   (action-set :initarg :action-set
	       :type [set]
	       :reader action-set
	       :documentation "If instantiated, holds the set of actions possible in this SMDP.")
   (trans-dist-fn :initarg :trans-dist
	       :type [cond-prob-dist]
	       :reader trans-dist-fn
	       :documentation "If instantiated, holds the transition distribution.")
   (avail-actions :initarg :avail-actions
		  :type function
		  :reader avail-actions-fn
		  :initform nil
		  :documentation "If instantiated, holds the avail actions function.")
   (cached-tabular-smdp :initform nil
			:accessor cached-tabular-smdp)
   (termination-predicate :initarg :term-pred
			  :type function
			  :reader term-pred
			  :initform (constantly nil)))
  (:documentation "abstract class for semi-Markov Decision Processes

Initargs for <smdp>
-------------------

:state-set - the state set.  If not provided, compute-state-set must be true

:compute-state-set - if this is true, the state set is computed upon creation as the recursive closure of init-set under transitions.  If this is true, the following three arguments should be provided.
:init-set - the initial state set.  Required
:key-fn - maps from elements to nonnegative integers (constantly 0 by default)
:max-key - max value that key-fn can take (0 by default)

:action-set - set of actions.  Superseded by the avail-actions-fn field if defined.
:avail-actions - function that takes in a state and outputs set of actions.
The default avail-actions method calls the avail-actions function if defined, or returns the action-set if its defined.  

:trans-dist - transition distribution.  Must be a dist over outcomes (S2 REW DUR) given (S . A).   Used by the default smdp-trans-dist method.

:term-pred - termination predicate.  Constantly nil by default.  The default terminal? method calls this.

:print-progress - how often to print progress when computing recursive closure (if compute-state-set is true).  nil by default.


Child classes will often override some of the methods, so not all the above initargs will be necessary.

"))


(setf (documentation #'state-set 'function) "state-set SMDP.  State space of this SMDP."
      (documentation #'action-set 'function) "action-set SMDP.  Set of actions of this SMDP (not all may be legal at a given state."
      (documentation #'term-pred 'function) "term-pred SMDP.  Termination predicate (constantly nil by default).")
  

(defmethod initialize-instance :after ((m <smdp>) &rest args &key (compute-state-set nil)
								  (init-set nil) (print-progress nil)
								  (key-fn (constantly 0))
								  (max-key 0))
  
  (flet ((successors (s)
	   (unless (terminal? m s)
	     (let ((act (avail-actions m s))
		   (succ nil))
	       (do-elements (a act (delete-duplicates succ))
		 (setf succ
		   (nconc succ (mapset 'list #'outcome-state (sample-space (smdp-trans-dist m s a))))))))))
    (when compute-state-set
      (setf (slot-value m 'state-set)
	(recursive-closure init-set #'successors :print-progress print-progress
			   :key-fn key-fn :max-key max-key)))))
      
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic operations on SMDPs for subclasses to implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric smdp-trans-dist (m s a)
  (:documentation "smdp-trans-dist SMDP STATE ACTION.  Transition distribution as a result of doing ACTION at STATE.  This is a distribution over outcomes, which consist of the next state, the reward, and the duration of the action.  Default method just calls the function stored in the trans-dist field on (cons s a).  Subclasses must either set this field or override this method.")
  (:method ((m <smdp>) s a) (funcall (trans-dist-fn m) (cons s a))))

(defgeneric terminal? (m s)
  (:documentation "terminal? SMDP STATE.  Return T iff STATE is a terminal state of SMDP, NIL otherwise.  The default is just to call the term-pred field, which in turn is always nil by default.")
  (:method ((m <smdp>) s) (funcall (term-pred m) s)))


(defgeneric avail-actions (m s)
  (:documentation "avail-actions SMDP STATE.  Return set of actions possible at this state.  The default method just calls the function stored in the avail-actions-fn field, or if that's not instantiated, the function action-set.  Subclasses must either set one of these fields or override this method.

There is an around method at the top level that returns #() at terminal states.
")
  (:method ((m <smdp>) s)
	   (aif (avail-actions-fn m)
		(funcall it s)
		(action-set m)))
  (:method :around ((m <smdp>) s)
	   (if (terminal? m s)
	       #()
	     (call-next-method))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline get-state-num get-action-num num-states))

(defun get-state-num (s m)
  (item-number s (state-set m)))

(defun get-action-num (a m)
  (item-number a (action-set m)))

(defun num-states (smdp)
  (size (state-set smdp)))