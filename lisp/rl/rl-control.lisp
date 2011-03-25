;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl/rl-control.lisp
;; defines <rl-control> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package reinforcement-learning)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rl-control> ()
  (
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; main components
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (env :type env-user:<env> 
	:reader env 
	:initarg :env)
   
   (observers :reader obs
	      :writer set-obs
	      :initarg :obs)
   
   (policy :reader policy
	   :type policy:[policy]
	   :initarg :policy)
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; params of current run
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (num-steps
    :type fixnum
    :reader num-steps
    :writer set-num-steps
    :initarg :num-steps
    :initform nil)
   
   (num-episodes
    :type fixnum
    :reader num-ep
    :writer set-num-ep
    :initarg :num-episodes
    :initform nil)
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; local state
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (elapsed-steps
    :type fixnum
    :accessor rlc-elapsed-steps)
   
   (elapsed-episodes
    :type fixnum
    :accessor rlc-elapsed-episodes)
   )
  
  (:documentation "Class that governs the behaviour of reinforcement learning algorithm.  Call the function reinforcement-learning with an object of this type as the argument.  Note that this class does *not* handle the specific updates that happen during learning - rather, it controls which actions are done in the environment, when and how the environment is reset, what events to inform learning algorithms about, when to stop learning, etc.  

Initargs
:env - <env>
:observers - either of type <observer> or a list of <observer>s
:policy - <policy>
:num-steps
:num-episodes


The kind of learning carried out by default is trajectory-based reinforcement learning.  Learning algorithms and other observers may assume that this works as follows : initially call inform-start-execution on all observers and reset the environment.  During execution, use policy to choose actions, and whenever the environment terminates, reset to a randomly chosen state.  Call inform-start-episode whenever a new environment begins, and inform-env-step after an environment step.  Terminate when either num-episodes is non-nil and is <= the number of elapsed episodes, or likewise for num-steps and elapsed steps.

To make other types of rl control frameworks, subclass this class and override the methods for begin-learning, done?, finish-learning, and/or generate-sample.  So long as they only interact with the environment through the provided functions start-new-episode, act-in-env, get-env-state, avail-env-actions, and env-terminated? subclasses don't need to worry about keeping track of the number of elapsed steps/episodes.  

"))


(defmethod initialize-instance :after ((rlc <rl-control>) &rest args)
  (declare (ignore args))
  (let ((o (obs rlc)))
    (unless (listp o) (set-obs (list o) rlc))))


(defvar *rlc* nil "Holds the last <rl-control> object on which reinforcement-learning was called.")


(define-condition finish-run (serious-condition)
  ()
  (:documentation "finish-run.  Signalled when we need to immediately stop running."))

   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro notify-all-observers (rlc msg &rest args)
  (with-gensyms (obs)
    `(dolist (,obs (obs ,rlc))
       (,msg ,obs ,@args))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function for reinforcement-learning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reinforcement-learning (rlc)
  "reinforcement-learning RL-CONTROL.  Executes in the environment, notifying observers of events that happen along the way.  Returns nothing.  The specific behaviour depends on rlc's class.  See documentation for <rl-control> and <es-rl>."  
  (begin-learning rlc)
  (setf *rlc* rlc)
  (handler-case
      (until (done? rlc)
	(generate-sample rlc))
    (finish-run () nil))
  (finish-learning rlc))
    
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic functions that subclasses can add methods for to 
;; change the behaviour of the rl control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric begin-learning (rlc)

  (:documentation "begin-learning RL-CONTROL.  Add method for this to affect what happens first when reinforcement-learning is called.  The default for <rl-control> simply resets the environment to a random state (and informs the observers).  There is also a :before method for <rl-control> (and therefore for all subclasses) that sends an inform-start-execution message to all observers, and resets elapsed-episodes and elapsed-steps")

  (:method :before ((rlc <rl-control>))
	   (notify-all-observers rlc inform-start-execution)
	   (setf (rlc-elapsed-episodes rlc) 0
		 (rlc-elapsed-steps rlc) 0))

  (:method ((rlc <rl-control>))
	   (start-new-episode rlc nil)))
  
  
(defgeneric done? (rlc)
  
  (:documentation "done? RL-CONTROL.  This is how the RL-CONTROL knows when to stop.  The default method returns true iff either (num-episodes rlc) is non-nil and <= (rlc-elapsed-episodes rlc) or (num-steps rlc) is non-nil and <= (num-steps rlc) is non-nil and <= (rlc-elapsed-steps rlc).")
  
  (:method ((rlc <rl-control>))
	   (or (awhen (num-ep rlc) (<= it (rlc-elapsed-episodes rlc)))
	       (awhen (num-steps rlc) (<= it (rlc-elapsed-steps rlc))))))


(defgeneric generate-sample (rlc)
  
  (:documentation "generate-sample RL-CONTROL.  This is what RL-CONTROL does on each sampling step.  The default method first resets the environment to a random state if it has terminated, then asks the policy for an action at the current state and does it (informing observers).")
  
  (:method ((rlc <rl-control>))
	   (when (env-terminated? rlc)
	     (start-new-episode rlc t))
	   (let ((a (handler-case
			(policy:make-choice (policy rlc) (get-env-state rlc))
		      (policy:unknown-state ()
			;; at unknown states, sample randomly from available actions
			(sample-uniformly (avail-env-actions rlc)))
		      (policy:choose-to-abort ()
			(error 'finish-run)))))
	     (act-in-env rlc a))))


(defgeneric finish-learning (rlc)
  
  (:documentation "finish-learning RL-CONTROL.  A hook for RL-CONTROLs to finish up after learning.  The default method does nothing, but there is a :after method for <rl-control> (and, therefore, all subclasses) that sends a inform-finish-execution message to all observers.")
  
  (:method ((rlc <rl-control>)) (values))
  
  (:method :after ((rlc <rl-control>))
	   (notify-all-observers rlc inform-finish-execution)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for subclasses to interact with environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-new-episode (rlc &optional (finishing-old-episode t))
  "start-new-episode RLC &optional (FINISHING-OLD-EPISODE t).  Reset env of rlc to a random nonterminal state and inform observers.  If FINISHING-OLD-EPISODE is true, the number of elapsed episodes will be incremented.."
  (let ((e (env rlc)))
    (env-user:reset e)
    (when finishing-old-episode
      (incf (rlc-elapsed-episodes rlc)))
    (let ((s (env-user:get-state e)))
      (notify-all-observers rlc inform-start-episode s))))

(defun act-in-env (rlc a)
  "act-in-env RLC A.  Do A in environment of RLC and inform observers.  Also inform them if the environment terminates.  Finally, increment the number of elapsed steps."
  (let ((e (env rlc)))
    (multiple-value-bind (r s2 term)
	(env-user:do-action e a)
      (notify-all-observers rlc inform-env-step a r s2 term)
      (incf (rlc-elapsed-steps rlc)))))


(defun env-terminated? (rlc)
  "env-terminated? RL-CONTROLLER.  Return t iff the environment has terminated."
  (env-user:at-terminal-state (env rlc)))

(defun avail-env-actions (rlc)
  "avail-env-actions RL-CONTROLLER.  Return set of available actions in environment."
  (env-user:get-actions (env rlc)))

(defun get-env-state (rlc)
  "get-env-state RL-CONTROLLER.  Return the state of the environment."
  (env-user:get-state (env rlc)))





(in-package cl-user)