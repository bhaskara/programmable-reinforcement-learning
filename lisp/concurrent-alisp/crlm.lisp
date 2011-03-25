;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; concurrent-alisp/crlm.lisp
;; Contains the main code that defines what it means to execute an calisp program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package calisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <crlm> ()
  (
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; the main components
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (env
    :type env-user:<env>
    :reader env
    :initarg :env)
   
   (partial-program
    :type <calisp-program>
    :reader part-prog
    :initarg :part-prog)
   
   (observers
    :reader observers
    :writer set-observers
    :initarg :observers)
   
   (debugging-observers
    :reader debugging-observers
    :writer set-debugging-observers
    :initarg :debugging-observers
    :initform nil)
   
   (policy
    :type policy:[policy]
    :reader policy
    :initarg :policy)
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; parameters of current run
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (num-steps
    :type fixnum
    :reader num-steps
    :initarg :num-steps
    :initform nil)
   
   (num-episodes
    :type fixnum
    :reader num-episodes
    :initarg :num-episodes
    :initform nil)
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; local state while executing
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (elapsed-steps
    :type fixnum
    :accessor crlm-elapsed-steps)
   
   (elapsed-episodes
    :type fixnum
    :accessor crlm-elapsed-episodes)
   
   (joint-state
    :reader joint-state
    :writer set-joint-state
    :initform (make-joint-state))
   
   (part-prog-terminated?
    :type boolean
    :documentation "True iff the partial program has terminated without the environment terminating."
    :accessor crlm-part-prog-terminated?)
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; thread-related bookkeeping
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (num-running-threads 
    :type fixnum
    :reader num-running-threads
    :accessor crlm-num-running-threads)

   (process-thread-ids 
    :documentation "maps lisp processes of each thread in the partial program to an ID"
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader process-thread-ids)
   
   (thread-states
    :documentation "maps thread ids to thread-state"
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :reader thread-states)
   
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; actions and effectors              
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (actions 
    :documentation "table that holds the joint action as it is built up.  maps effector to individual action"
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader actions)
   
   (effector-thread-ids
    :documentation "table mapping effector to id of thread currently holding it."
    :type hash-table 
    :initform (make-hash-table :test #'eq)
    :reader effector-thread-ids)
   
   (reassign-list
    :documentation "list of effector-thread pairs that represent reassignments that must happen at the beginning of the next env step."
    :type list :accessor reassign-list)
   
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; choices
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (thread-choice-results
    :documentation "table holding the joint choice made for a subset of the choice threads"
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :reader thread-choice-results)
   
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; locks and condition variables
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (lock
    :type process-lock
    :initform (make-process-lock :name "CRLM Lock")
    :reader lock)
   
   (action-condition
    :type <condition-variable>
    :reader action-condition
    :writer set-action-condition)
   
   (choice-condition
    :type <condition-variable>
    :reader choice-condition
    :writer set-choice-condition)
   
   (wait-condition
    :type <condition-variable>
    :reader wait-condition
    :writer set-wait-condition)
   
   (step-condition
    :type <condition-variable>
    :reader step-condition
    :writer set-step-condition)
   
   (effector-condition
    :type <condition-variable>
    :reader effector-condition
    :writer set-effector-condition)
   )
   
   
  (:documentation "Class that basically serves as a place to put all the relevant local state while running a concurrent ALisp program.  Required initargs are
:env - <env>
:part-prog - <part-prog>
:policy - <calisp-policy>
:observers - either of type <calisp-observer> or a list of <calisp-observer>s" ))
   


(defmethod initialize-instance :after ((c <crlm>) &rest args)
  (declare (ignore args))
  (let ((observers (observers c)))
    (if (listp observers)
	(dolist (obs observers)
	  (check-type obs rl:<rl-observer>))
      (progn
	(check-type observers rl:<rl-observer>)
	(set-observers (list observers) c))))
  (let ((deb-observers (debugging-observers c)))
    (unless (listp deb-observers)
      (check-type deb-observers <calisp-debugging-observer>)
      (set-debugging-observers (list deb-observers) c)))
  (let ((lock (lock c)))
    (set-action-condition (make-instance '<condition-variable> :lock lock :name "action-cv" ) c)
    (set-choice-condition (make-instance '<condition-variable> :lock lock :name "choice-cv" ) c)
    (set-wait-condition (make-instance '<condition-variable> :lock lock :name "wait-cv") c)
    (set-effector-condition (make-instance '<condition-variable> :lock lock :name "effector-cv") c)
    (set-step-condition (make-instance '<condition-variable> :lock lock :name "step-cv" ) c)
    ))
  
  

(define-condition crlm-last-step-reached ()
  ())

(define-condition env-terminated ()
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro notify-debugging-observers (crlm msg &rest args)
  "notify-debugging-observers CRLM MSG &rest ARGS.  Notify debugging observers of MSG.  The joint state is also cloned and inserted before the ARGS."
  (with-gensyms (observers obs cloned-state)
    `(let ((,observers (debugging-observers ,crlm)))
       (when ,observers
	 (let ((,cloned-state (clone (joint-state ,crlm))))
	   (dolist (,obs ,observers)
	     (,msg ,obs ,cloned-state ,@args)))))))

(defmacro notify-all-observers (crlm msg &rest args)
  "notify-all-observers CRLM MSG &rest ARGS.  Notify all observers (including debugging observers) of MSG with ARGS."
  (with-gensyms (obs)
    `(progn
       (dolist (,obs (observers ,crlm))
	 (,msg ,obs ,@args))
       (dolist (,obs (debugging-observers ,crlm))
	 (,msg ,obs ,@args)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code that executes in the control thread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(defun run (c &aux (lock (lock c)))
  "run CRLM.  Repeatedly run the partial program in the environment."
  
  ;; during this run, dynamically bind the *crlm* special variable to crlm
  ;; for debugging purposes, for now just setf *crlm*
  (setf *crlm* c
	(crlm-elapsed-steps c) 0
	(crlm-elapsed-episodes c) 0)

  (notify-all-observers c inform-start-execution)
  
  (with-process-lock (lock)
    (handler-case
      
	;; loop.  One iteration per episode.  Exited when a
	;; crlm-last-step-reached condition is signalled.
	(loop
	
	  (unwind-protect
	    
	      ;; protected form : start up episode, then loop, doing a crlm step each time.
	      ;; Will exit when either an env-terminated error is signalled
	      ;; (which is handled below), or the last step is reached
	      (progn

		;; reset env, start part prog, set up bookkeeping vars
		(start-episode c)
	
		;; run episode
		(handler-case
		  
		    ;; loop with one iteration per step.  Exited
		    ;; when env terminates, or crlm-last-step-reached signalled.
		    (loop
	  
		      ;; wait for step
		      (until (no-running-threads c)
			(notify-debugging-observers c inform-main-thread-wait)
			(wait (step-condition c)))
		      (notify-debugging-observers c inform-main-thread-wakeup)
		    
		      ;; take next step
		      (if (at-env-action c)
			  (joint-action c)
			(joint-choice c)))
		
		  ;; If we stopped because of env termination, check if this was the 
		  ;; last episode.
		  (env-terminated ()
		    (let ((elapsed-episodes (incf (crlm-elapsed-episodes c))))
		      (when (aand (num-episodes c) (>= elapsed-episodes it))
			(error 'crlm-last-step-reached))))
		  
		  ;; Otherwise, we must have stopped because of reaching the last step
		  ;; of the main loop.  Set a flag, and pass on the condition.
		  (crlm-last-step-reached (c2)
		    (setf (crlm-part-prog-terminated? c) t)
		    (error c2))))
	  
	    ;; cleanup form : regardless of why we stopped, kill all threads.
	    ;; This will cause each thread to unwind, and then stop.
	    (kill-all-threads c)))
    
      ;; when last step is reached, notify observers, then exit
      (crlm-last-step-reached ()
	(notify-all-observers c inform-finish-execution)))))
	
	

(defun start-episode (c &aux (root-thread-id (root-thread-id (part-prog c))))
  "reset env state, create root thread, set up bookkeeping vars"
  
  ;; reset env
  (env-user:reset (env c))
  
  ;; reset condition vars
  (dolist (cv (list (action-condition c) 
		    (choice-condition c) 
		    (step-condition c) 
		    (effector-condition c) 
		    (wait-condition c)))
    (remove-waiting-processes cv))
  
  (let ((proc (make-process :name (format nil "~a" root-thread-id))) ;; root thread
	(effectors (current-effectors c))
	(actions (actions c))
	(eff-t-ids (effector-thread-ids c)))
    
    ;; set up bookkeeping vars
    (setf (crlm-num-running-threads c) 1
	  (crlm-part-prog-terminated? c) nil
	  (gethash proc (clrhash (process-thread-ids c))) root-thread-id
	  
	  (gethash root-thread-id (clrhash (thread-states c)))
	  (make-thread-state :status 'running :effectors effectors)
	  
	  (gethash root-thread-id (clrhash (thread-choice-results c)))
	  'choice-unassigned)
    
    (let ((ts (gethash root-thread-id (thread-states c))))
      (push (make-frame 'start) (ts-stack ts)))
    
    (clrhash actions)
    (clrhash eff-t-ids)
    (setf (reassign-list c) nil)
    (do-elements (eff effectors)
      (setf (gethash eff actions) 'action-unassigned
	    (gethash eff eff-t-ids) root-thread-id))
    
    
    (let ((s (env-user:get-state (env c))))
      (set-joint-state 
       (make-joint-state :env-state (env-user:get-state (env c))
			 :global (make-frame 'global)
			 :thread-states (thread-states c))
       c)
      (notify-all-observers c inform-start-episode s))
    
    
    

    
    ;; start root thread
    (initialize-thread c proc #'start (list (part-prog c)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joint-action and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun joint-action (c &aux (env (env c)))
  
  (assert (am-holding-lock c) nil
    "joint-action called while not holding lock.")
  
  (let* ((actions (actions c))
	 (act (mapset 'list (lambda (eff) (cons eff (gethash eff actions))) (current-effectors c)))
	 (s (env-user:get-state env))
	 (old-effs (current-effectors c)))
    
    
    ;; check that env hasn't terminated
    (assert (not (env-user:at-terminal-state env)) nil 
      "Tried to do action ~a but environment is at a terminal state ~a"
      act s)

    ;; perform the action in the environment
    (multiple-value-bind (r next-s term)
	(env-user:do-action env act)
      
      ;; update the env state
      (set-env-state next-s c)
      
      ;; inform all observers
      (notify-all-observers c inform-env-step act r next-s term)
      
      ;; keep track of effectors that have been added or deleted
      ;; in the new state
      (handle-effector-changes c old-effs (current-effectors c))
      
      (handle-pending-reassigns c)
      
      ;; initialize action table to all unassigned
      (let ((actions (actions c)))
	(maphash 
	 (lambda (k v)
	   (declare (ignore v))
	   (setf (gethash k actions) 'action-unassigned))
	 actions))
      
      
      ;; thread bookkeeping
      (loop
	  for ts being each hash-value in (thread-states c)
	  when (eq (ts-status ts) 'waiting-to-act)
	  do (make-thread-state-internal ts))

      
      
      ;; Increment number of steps
      (let ((steps (incf (crlm-elapsed-steps c))))
	
	;; Has the env terminated?
	(when (env-user:at-terminal-state env)
	  (error 'env-terminated))
	
	;; Have we reached last step?
	(when (aand (num-steps c) (>= steps it))
	  (error 'crlm-last-step-reached)))
      
      
      ;; If not, wake up threads waiting for an environment step
      (wake-up-cpp-threads c (action-condition c))
   
      ;; return nothing
      (values))))


(defun handle-pending-reassigns (c )
  
  (assert (am-holding-lock c) nil
    "handle-pending-reassigns called while not holding lock.")
  
  
  (let ((eff-t-ids (effector-thread-ids c)))
    (dolist (entry (reassign-list c))
      
      (destructuring-bind (id . eff) entry
	(let ((src-id (check-not-null (gethash eff eff-t-ids)
				      "Eff-t-id entry for ~a" eff))
				      
	      (dest-state (check-not-null (gethash id (thread-states c))
					  "Thread state for ~a" id)))
	  (setf (gethash eff eff-t-ids) id)
	  (let* ((src-state (check-not-null (gethash src-id (thread-states c))
					    "Thread state for id ~a" src-id))
		 (src-effectors (ts-effectors src-state)))
	    (assert (member eff src-effectors :test #'equal) ()
	      "Unexpectedly, effector ~a did not belong to effector list ~a of thread ~a before reassign."
	      eff src-effectors src-id)
	    (setf (ts-effectors src-state) (delete eff src-effectors :test #'equal))
	    (push eff (ts-effectors dest-state)))))))
  
  (setf (reassign-list c) nil))
    
    

(defun handle-effector-changes (c old-effs new-effs)
  "handle-effector-changes CRLM OLD-EFFS NEW-EFFS.  Do the necessary bookkeeping to figure what effectors are new/removed, and call the partial program's assign-new-effectors method to figure out which thread gets the new effectors."
  (multiple-value-bind (added deleted)
      (symmetric-difference new-effs old-effs)
    (let ((eff-t-ids (effector-thread-ids c))
	  (actions (actions c))
	  (thread-states (thread-states c)))
      (do-elements (e deleted)
	(let* ((ts (gethash (gethash e eff-t-ids) thread-states))
	       (effs (ts-effectors ts)))
	  (assert (member e effs) ()
	    "Unexpectedly, effector ~a was not in effector list ~a of thread ~a"
	    e effs (gethash e eff-t-ids))
	  (setf (ts-effectors ts) (delete e effs))
	  )
	(remhash e eff-t-ids)
	(remhash e actions))
      
      
      (let ((ids (assign-effectors (part-prog c) (joint-state c) added))
	    (t-states (thread-states c)))
	(mapc (lambda (eff id)
		  (multiple-value-bind (t-state exists?)
		      (gethash id t-states)
		    (assert exists? ()
		      "Attempted to assign new effector ~a to nonexistent thread ~a.  Actual set of threads is ~a."
		      eff id (hash-keys t-states))
		    (setf (gethash eff actions) 'newly-added
			  (gethash eff eff-t-ids) id)
		    (push eff (ts-effectors t-state))))
		    
	      added ids)))))

					       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joint choice and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun joint-choice (c)
  
  (assert (am-holding-lock c) nil 
    "joint-choice called while not holding crlm lock.")
  
  ;; figure out which threads are actually choosing
  (let* ((omega (joint-state c))
	 (choosing-thread-ids (setf (js-choosing-thread-ids omega)
			     (choosing-thread-ids (part-prog c) omega))))
    (assert choosing-thread-ids nil "Empty list of labels of choosing threads at ~a" omega)

    ;; make sure threads exist and want to choose.  set their status.
    (dolist (id choosing-thread-ids)
      (multiple-value-bind (ts present)
	  (gethash id (thread-states c))
	(let ((stat (ts-status ts)))
	  (assert (and present (eq stat 'holding)) nil
	    "Invalid thread id ~a with status ~a" id stat))
	(setf (ts-status ts) 'choosing)))
    
    ;; make fresh copy of joint state to pass to observers
    (let ((fresh-omega (clone omega))
	  (choices (choice-set omega choosing-thread-ids))
	  (choice-res (thread-choice-results c)))
      (setf (js-choices fresh-omega) choices)
      
      ;; inform observers that we have arrived at a choice
      (notify-all-observers c inform-arrive-choice-state fresh-omega)
      
      ;; use policy to make the choice.  If the policy declines, stop execution.
      ;; if the state is unknown to the policy, choose randomly instead.
      (let ((choice
	     (handler-case
		 (policy:make-choice (policy c) fresh-omega)
	       (policy:choose-to-abort ()
		 (error 'crlm-last-step-reached))
	       (policy:unknown-state ()
		 (sample-uniformly choices)))))
	
	;; make sure this choice is legal
	(assert (set:member? choice choices)
	    (choice) "Choice ~a is not a member of ~a" choice choices)
      
	;; store the choice that was made.  Assumes choice is an
	;; association list from thread-id to choice
	(mapc #'(lambda (choice-entry)
		  (setf (gethash (car choice-entry) choice-res)
		    (cdr choice-entry)))
	      choice)
	    
	;; inform observers
	(notify-all-observers c inform-calisp-step fresh-omega choice))
      
      
      ;; wake up cpp threads
      (wake-up-cpp-threads c (choice-condition c))
      (wake-up-cpp-threads c (wait-condition c)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations called in threads of the partial program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       

(defmacro call (fn-name args &key (label fn-name))
  "Macro call FN-NAME ARGS &key (LABEL FN-NAME)

FN-NAME (not evaluated) is the name of the function to be called.
ARGS (not evaluated) is a lambda list, in which some elements may be of the form (choose-arg CHOICE-LIST) where CHOICE-LIST is evaluated."
  (let ((num-unspecified 0) ;; how many vars are unspecified
	(num-params (length args))
	(choice-lists nil) ;; list of choices for each unspecified param
	(unspecified (make-array 0 :adjustable t :fill-pointer 0)) ;; list where nth element is t if nth param unspecified
	(unspecified-param-nums (make-array 0 :adjustable t :fill-pointer 0)) ;; list of numbers of unspecified params
	(exit-label (intern-compound-symbol label "-EXIT")) ;; label of exit point of this call
	(arg-vars ;; avoid multiple evaluation of arguments
	 (loop repeat (length args) collect (gensym))))

    
    ;; preprocessing to figure out at compile-time which parameters are specified
    (loop
	for a in args
	for i below num-params
	for unspec = (and (listp a) (eq (first a) 'choose-arg))
		     
	do (vector-push-extend unspec unspecified)
	when unspec
	do (incf num-unspecified)
	   (push (second a) choice-lists)
	   (vector-push-extend i unspecified-param-nums))

    
    (with-gensyms (c new-frame choice param-num param-choice lock current-thread-id
		     choice-list-vals ts var val param-names cloned-state)

      ;; this is where the expanded code begins
      `(let ((,c *crlm*)
	     (,lock (lock *crlm*))
	     (,new-frame (make-frame ',fn-name))
	     ,@(when choice-lists
		 `((,choice-list-vals (list ,@(reverse choice-lists)))))

	     ;; bind vars for args to avoid multiple evaluation	      
	     ,@(map 'list (lambda (a v u) `(,v ,(if u ''unspecified a))) args arg-vars unspecified)
	     )
	 
	 (with-process-lock (,lock)
	   ;; get thread state
	   (multiple-value-bind (,current-thread-id ,ts)
	       (lookup-process ,c)
	   
	     ;; update the program counter and state type
	     (update-program-counter ,ts ',label)
	     (setf (frame-label (first (ts-stack ,ts))) ',label)

	 
	     ;; add new frame
	     (setf (ts-next-frame ,ts) ,new-frame)

	     (let ((,param-names (lookup-calisp-subroutine #',fn-name)))
	     
	       (mapc (lambda (,var ,val)
		       (set-frame-var-val ,new-frame ,var ,val nil))
		     ,param-names (list ,@arg-vars))
	 
	       ;; set the choice list and state type
	       (setf (ts-choices ,ts)
		 ,(cond ((= num-unspecified 1) `(first ,choice-list-vals))
			((> num-unspecified 0) choice-list-vals)
			(t `'(no-choice)))
		 (ts-type ,ts) 'call)
	 
	       ;; make choice
	       (let* ((,choice (choose-using-completion ,c)))
	 
		 ;; fill in unspecified parameters into frame
		 ,(case num-unspecified
		    (0 `(declare (ignore ,choice)))
		    (1 `(set-frame-var-val ,new-frame (nth ,(aref unspecified-param-nums 0) ,param-names) ,choice t))
		    (t `(map nil (lambda (,param-num ,param-choice)
				   (set-frame-var-val ,new-frame (nth ,param-num ,param-names) ,param-choice t))
			     ,unspecified-param-nums ,choice)))
	 
		 ;; move next frame to top of stack
		 (move-next-frame-to-stack ,ts)
	 
		 ;; fill in unspecified arguments of function call
		 ,(cond ((= num-unspecified 1)
			 `(setf ,(nth (aref unspecified-param-nums 0) arg-vars) ,choice))
			((> num-unspecified 1)
			 `(setf
			      ,@(loop
				    for i across unspecified-param-nums
				    collect (nth i arg-vars)
				    collect `(nth ,i ,choice))))))
	 
	       ;; do the function call (and return this as the final return value)
	       (unwind-protect 
		   (,fn-name ,@arg-vars)
	   
		 ;; cleanup forms
		 ;; pop stack, set pc to exit point of this choice
		 (pop (ts-stack ,ts))
		 (update-program-counter ,ts ',exit-label)
		 (setf (ts-choices ,ts) '(no-choice)
		       (ts-type ,ts) 'call-exit)
	       
		 (let ((,cloned-state (clone (joint-state ,c))))
		   (unless (crlm-part-prog-terminated? ,c)
		   (notify-all-observers ,c inform-end-choice-block 
					 ,current-thread-id ,cloned-state)))
	     
		 ;; update state again and exit the unwind-protect
		 (make-thread-state-internal ,ts)
		 )
	       )))))))
    


(defmacro choose (label &rest choices)
  "macro choose LABEL &rest CHOICES
LABEL (not evaluated) : label for this choice point
CHOICES (not evaluated) : list, where either all choices are of the form (CHOICE-NAME CHOICE-FORMS) where CHOICE-NAME is not the symbol 'call, or all choices are of the form (CALL &rest ARGS) where ARGS would be the arguments to a CALL statement."
  
  (let ((choice-labels (map 'vector
			 (lambda (x)
			   (if (eq (first x) 'call)
			       (caadr x)
			     (car x)))
			 choices))
	(forms (mapcar (lambda (x) (if (eq (first x) 'call) (second x)
				     `(progn ,@(rest x)))) choices)))
    
    (with-gensyms (c new-frame choice ts lock cloned-state current-thread-id)
      
      ;; expanded code starts here
      `(let ((,c *crlm*)
	     (,lock (lock *crlm*))
	     (,new-frame (make-frame ',label)))

	 (with-process-lock (,lock)
	 
	   ;; get thread state
	   (multiple-value-bind (,current-thread-id ,ts)
	       (lookup-process ,c)
	 
	     ;; update program counter
	     (update-program-counter ,ts ',label)
	     (setf (frame-label (first (ts-stack ,ts))) ',label)
	   
	     ;; add new frame
	     (setf (ts-next-frame ,ts) ,new-frame)
	 
	     ;; add single entry to frame for choice-var
	     ;; (set-frame-var-val ,new-frame 'choice 'unspecified nil)
	 
	     ;; set choice set, type
	     (setf (ts-choices ,ts) ',choice-labels
		   (ts-type ,ts) 'choose)
	 
	     ;; make choice and binding
	     (let ((,choice (choose-using-completion ,c)))

	       ;; fill in unspecified parameter into frame
	       ;; (set-frame-var-val ,new-frame 'choice ,choice t)
	       
	       ;; rename frame with label of the choice
	       (setf (frame-name ,new-frame) ,choice)
	 
	       ;; move next frame to top of stack
	       (move-next-frame-to-stack ,ts)
	     
	       (make-thread-state-internal ,ts)
	 
	       ;; evaluate chosen form
	       (unwind-protect
		   (case ,choice
		     ,@(loop
			   for f in forms
			   for ch across choice-labels
			   collect `(,ch ,f)))

		 ;; cleanup forms
		 ;; pop stack, set pc to exit point of this choice
		 (pop (ts-stack ,ts))
		 (update-program-counter ,ts ',(intern-compound-symbol label "-EXIT"))
		 (setf (ts-choices ,ts) '(no-choice)
		       (ts-type ,ts) 'choose-exit)
	       
		 (let ((,cloned-state (clone (joint-state ,c))))
		   (unless (crlm-part-prog-terminated? ,c)
		   (notify-all-observers ,c inform-end-choice-block 
					 ,current-thread-id ,cloned-state)))
	   
		 ;; update state again and exit unwind-protect
		 (make-thread-state-internal ,ts)
		 )
	       )))))))
	       
(defmacro dummy-choice (label)
  "macro dummy-choice LABEL.  Set up a dummy choice point with label LABEL with the single choice labelled 'no-choice that expands to nil."
  `(choose ,label ((no-choice (nil)))))

	 



(defmacro with-choice (label (var choices) &body body)
  "with-choice LABEL (VAR CHOICES) &body BODY
LABEL (not evaluated) - label of this choice
VAR (not evaluated) - symbol that names the choice variable
CHOICES (evaluated) - set of choices
BODY (not evaluated) - set of forms enclosed by implicit progn

Bind VAR to a value chosen from CHOICES by the completion, then execute BODY."

  (with-gensyms (c new-frame ts lock current-thread-id cloned-state actual-choices)
    ;; expanded code starts here
    `(let ((,c *crlm*)
	   (,lock (lock *crlm*))
	   (,new-frame (make-frame ',label))
	   (,actual-choices ,choices)
	   )
       
       (with-process-lock (,lock)
       
	 ;; get current thread state
	 (multiple-value-bind (,current-thread-id ,ts)
	     (lookup-process ,c)
	 
	   ;; update the program counter
	   (update-program-counter ,ts ',label)
	   (setf (frame-label (first (ts-stack ,ts))) ',label)	 
	 
	 
	   ;; add new frame
	   (setf (ts-next-frame ,ts) ,new-frame)
     
	   ;; add a single entry to this frame for the choice variable
	   (set-frame-var-val ,new-frame ',var 'unspecified nil)

	   ;; set the choice list and type
	   (setf (ts-choices ,ts) ,actual-choices
		 (ts-type ,ts) 'with-choice)
       
	   ;; make choice and binding
	   (let ((,var (choose-using-completion ,c)))
	 
	     ;; fill in unspecified parameter into frame
	     (set-frame-var-val ,new-frame ',var ,var t)
       
	     ;; move next frame to top of stack
	     (move-next-frame-to-stack ,ts)
	 
	     ;; do function call (and return this as final return value)
	     (unwind-protect
		 (progn ,@body)
	     
	       ;; cleanup forms
	       ;; pop stack, set pc to exit point of this choice
	       (pop (ts-stack ,ts))
	       (update-program-counter ,ts ',(intern-compound-symbol label "-EXIT"))
	       (setf (ts-choices ,ts) '(no-choice)
		     (ts-type ,ts) 'with-choice-exit)
	     
	       (let ((,cloned-state (clone (joint-state ,c))))
		 (unless (crlm-part-prog-terminated? ,c)
		   (notify-all-observers ,c inform-end-choice-block 
					 ,current-thread-id ,cloned-state)))
	   
	       ;; update state again and exit unwind protect
	       (make-thread-state-internal ,ts)
	       )
	     ))))))



(defun choose-using-completion (c)
  (assert (am-holding-lock c))
  (let ((choice-res (thread-choice-results c)))

    ;; get relevant info about this thread
    (multiple-value-bind (current-thread ts)
	(lookup-process c)
      (let ((pc (ts-pc ts))
	    (choices (ts-choices ts)))
	
	;; thread bookkeeping
	(setf (ts-status ts) 'holding
	      (gethash current-thread choice-res) 'choice-unassigned)
	
	;; a loop is needed because not all threads will choose at a choice state
	(while (eq (gethash current-thread choice-res) 'choice-unassigned)
	  
	  ;; wake up control thread if at step state
	  (dec-num-running-threads c)
	  (when (no-running-threads c)
	    (notify-all (step-condition c)))
	  
	  ;; go to sleep till choice is made
	  (notify-debugging-observers c inform-wait-choice current-thread pc)
	  (wait (choice-condition c)))
	
	
	(notify-debugging-observers c inform-wakeup current-thread)
	
	;; return the choice that was made
	(multiple-value-bind (val present)
	    (gethash current-thread choice-res)
	  (assert (and present (member? val choices)) (val)
	    "Thread ~a received invalid choice ~a at choice point ~a"
	    current-thread val pc)
	  val)))))



(defun lookup-calisp-subroutine (f)
  (let ((l (get-lambda-list f)))
    (assert (notany
	     (lambda (x) (member x '(&optional &key &rest &aux)))
	     l)
	() "Concurrent ALisp subroutines cannot have optional, key, rest, or aux arguments.")
    l))
       




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro action (&rest args)
  "Macro action [LABEL] ACTION.

LABEL (not evaluated) - label for this point in the calisp program.  Defaults to nil.
ACTION - any object.

Called by a thread to cause that thread's effectors to perform the given actions.  ACTION is either a list of pairs, in which case it is treated as an association list from effectors to their individual actions, or not, in which case it is treated as an individual action that is done by each effector."
  
  (if (= 1 (length args))
      `(act-crlm *crlm* ,(first args) nil)
    `(act-crlm *crlm* ,(second args) ',(first args))))

(defun act-crlm (c act label &aux (lock (lock c)))
  (with-process-lock (lock)
    (multiple-value-bind (id ts)
	(lookup-process c)
      
      (let* ((effectors (ts-effectors ts))
	     (actions (if (and (listp act) (every #'consp act)) act 
			(mapcar (lambda (e) (cons e act)) effectors))))
	
	;; checks
	(assert
	    (and (= (length effectors) (length actions))
		 (every (lambda (action) (member (first action) effectors))
			actions))
	    nil
	  "Thread called action with action list ~a but the actual effector list is ~a"
	  actions effectors)
	
	;; thread bookkeeping
	(setf (ts-status ts) 'waiting-to-act)
	(update-program-counter ts label)
	(setf (frame-label (first (ts-stack ts))) label)	
	
	;; set actions for the effectors
	(dolist (a actions)
	  (setf (gethash (car a) (actions c)) (cdr a)))
	
	;; Wake up crlm thread either if all effectors have been assigned, or num-running-threads = 0
	;; The second condition is for joint choices, and the first condition is necessary when 
	;; not all threads possess effectors
	(dec-num-running-threads c)
	(when (or (no-running-threads c)
		  (at-env-action c))
	  (notify-all (step-condition c)))
	
	(notify-debugging-observers c inform-wait-action id label)
	
	(setf (ts-type ts) 'action)
	
	;; Wait for action to complete, then return
	(wait (action-condition c))
	
	(notify-debugging-observers c inform-wakeup id)
	
	
	;; return nothing
	(values)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro spawn (id func &rest a)
  "spawn ID FUNC [LABEL] ARGS EFFECTORS.

ID (evaluated) - ID of new thread.
FUNC (not evaluated) - name of function to call
LABEL (not evaluated) - a symbol that is the label of this location in the program
ARGS (evaluated) - arg list
EFFECTORS (evaluated) - effector list of new thread."
  
  (condlet
      (((= (length a) 3)
	(label (first a))
	(args (second a))
	(effectors (third a)))
       ((= (length a) 2)
	(label func)
	(args (first a))
	(effectors (second a)))
       (otherwise (assert nil nil "Incorrect argument list ~a to spawn"
			  (list* id func a))))
    `(spawn-crlm *crlm* ',label ,id #',func ,args ,effectors (make-frame ',func))))



(defun spawn-crlm (c label id func args effectors new-frame &aux (lock (lock *crlm*)))
  (with-process-lock (lock)
    
    (multiple-value-bind (current-id current-ts)
	(lookup-process c)
      (declare (ignore current-id))
      
      (let ((process-name (format nil "~a" id)) ; because process name must be a string
	    (thread-states (thread-states c)))

	;; checks
	(assert (not (hash-table-has-key thread-states id)) (id)
	  "Attempted to spawn a thread with id ~a, which already exists" id)
      
	;; update the program counter and state type
	(update-program-counter current-ts label)
	(setf (frame-label (first (ts-stack current-ts))) label
	      (ts-choices current-ts) '(no-choice)
	      (ts-type current-ts) 'spawn)

	;; wait for spawn to happen
	(choose-using-completion c)
	
	;; start the new thread
	(let ((process (make-process :name process-name))
	      (t-state (make-thread-state :status 'internal)))
	  (push new-frame (ts-stack t-state))
	  (setf (gethash process (process-thread-ids c)) id
		(gethash id thread-states) t-state)
	  (incf (crlm-num-running-threads c))
	  (multiple-value-bind (current-thread current-thread-state)
	      (lookup-process c)
	    (declare (ignore current-thread-state))
	    (notify-all-observers c inform-spawn-thread id current-thread effectors))
	  (reassign-crlm c id effectors)
	  (initialize-thread c process func args)
	  (values))))))


(defun get-new-thread-id (id)
  "get-new-thread-id ID.  
ID is a symbol.
Look in the thread table of *crlm*, and return a list of the form (ID N) which does not name an existing thread, where N is an integer."
  (list id
	(1+
	 (loop
	     for k being each hash-key in (thread-states *crlm*)
	     maximize
	       (if (and (listp k) (eq (first k) id))
		   (second k)
		 -1)))))


(defun reassign-crlm (c id eff 
		      &aux (lock (lock c))
			   (effectors (if (listp eff) eff (list eff))))
  (with-process-lock (lock)
    (multiple-value-bind (src-thread src-thread-state)
	(lookup-process c)
      
      (let ((my-effectors (ts-effectors src-thread-state)))
	(multiple-value-bind (dest-thread-state dest-thread-exists)
	    (gethash id (thread-states c))
	  
	  ;; checks
	  (assert dest-thread-exists nil 
	    "Attempted to reassign ~a to unknown thread ~a" effectors id)
	  (assert (every (lambda (eff) (member eff my-effectors)) effectors) nil
	    "Not every effector in ~a is part of ~a's effector list ~a"
	    effectors src-thread my-effectors)
	  
	  (let ((his-effectors (ts-effectors dest-thread-state)))
	    
	    ;; Reassign
	    (dolist (eff effectors)
	      (setf my-effectors (delete eff my-effectors)
		    (gethash eff (effector-thread-ids c)) id
		    his-effectors (insert-sorted-list eff his-effectors)))
	    (setf (ts-effectors src-thread-state) my-effectors
		  (ts-effectors dest-thread-state) his-effectors))
	  
	  ;; Notify threads who may be waiting for effectors
	  (wake-up-cpp-threads c (effector-condition c))
	  (wake-up-cpp-threads c (wait-condition c))
	  
	  (notify-all-observers c inform-reassign effectors src-thread id)
	  
	  (values))))))


(defun die (c &aux (lock (lock c)))
  
  (with-process-lock (lock)
    
    (multiple-value-bind (current-thread current-thread-state)
	(lookup-process c)

      ;; remove this thread info from the tables
      (remhash *current-process* (process-thread-ids c))
      (remhash current-thread (thread-states c))
      (remhash current-thread (thread-choice-results c))
      
      (unless (crlm-part-prog-terminated? c)
	
	(notify-all-observers c inform-die-thread current-thread)
	(dec-num-running-threads c)

	(unless (env-user:at-terminal-state (env c))
	  
	  ;; checks
	  (assert (null (ts-effectors current-thread-state))
	      nil "Thread ~a called die while still holding effectors ~a"
	      current-thread (ts-effectors current-thread-state))
	  (assert (> (hash-table-count (thread-states c)) 0) nil
	    "All threads died before environment terminated.")
      
	  ;; if this thread dying causes us to be at a step state, notify crlm thread
	  (when (no-running-threads c)
	    (notify-all (step-condition c)))))
      (values))))


(defmacro reassign (label &rest args)
  "Macro reassign LABEL EFFECTORS DEST-THREAD-ID &key WAIT-ACTION

LABEL (not evaluated) - label of this point in the program
EFFECTORS (evaluated) - list of effectors to reassign
DEST-THREAD-ID (evaluated) - id of destination thread
WAIT-ACTION (evaluated) - when supplied, indicates that, if the destination thread has already committed to a joint action, then the effectors being reassigned will do this action (via a call to ACTION) on this step, and be reassigned at the beginning of the next step.  If this argument is not supplied, assert in the above situation."
  `(reassign-to-existing-thread *crlm* ',label ,@args))

(defun reassign-to-existing-thread (c label effectors id &key (wait-action nil wait-action-supplied)
				    &aux (lock (lock c)))
							      
  
  (with-process-lock (lock)
    ;; get state of dest thread
    (multiple-value-bind (t-state exists)
	(gethash id (thread-states c))
      (assert exists () "Attempted to reassign effectors ~a to nonexistent thread ~a.  Actual thread list is ~a." 
	      effectors id (hash-keys (thread-states c)))
    
      (case (ts-status t-state)

	;; if dest is already at an action, wait for a step
	(waiting-to-act
	 (assert wait-action-supplied ()
	   "Attempted to reassign effectors ~a to thread ~a which is already committed to an action, and the reassign call did not specify a default action." effectors id)
	 (dolist (eff effectors)
	   (push (cons id eff) (reassign-list c)))
	 (act-crlm c wait-action label)
	 )
	
	;; otherwise assign immediately
	(otherwise (reassign-crlm c id effectors))))))
    
    
  

(defmacro wait-for-effectors (label)
  "wait-for-effectors LABEL

LABEL (not evaluated) is a symbol labelling this program state."
  `(wait-effectors-crlm *crlm* ',label))

(defun wait-effectors-crlm (c label)
  (let ((lock (lock *crlm*)))
    (with-process-lock (lock)
      (multiple-value-bind (id ts)
	  (lookup-process *crlm*)
	(setf (ts-status ts) 'waiting-for-effector
	      (ts-type ts) 'internal
	      (ts-choices) 'not-choosing)
	(update-program-counter ts label)
	(until (my-effectors)
	  
	  (dec-num-running-threads c)
	  (when (no-running-threads c)
	    (notify-all (step-condition c)))
	  (notify-debugging-observers c inform-wait-effectors id)
	  (wait (effector-condition c)))
	
	(notify-debugging-observers c inform-wakeup id)
	(make-thread-state-internal ts)))))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessing the joint state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-effectors ()
  "my-effectors.  Return the effector list of the calling thread."
  (multiple-value-bind (id ts)
      (lookup-process *crlm*)
    (declare (ignore id))
    (ts-effectors ts)))

(defun env-state ()
  "env-state.  Return environment state."
  (get-env-state *crlm*))

(defun combined-state ()
  "combined state.  Return combined state.  Called within partial programs.  Returned value should not be modified in any way."
  (joint-state *crlm*))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline am-holding-lock wake-up-cpp-threads dec-num-running-threads no-running-threads))

(defun dec-num-running-threads (c)
  "Decrement num running threads."
  (decf (crlm-num-running-threads c))
  (values))

(defun no-running-threads (c)
  "Return t iff there are no running threads."
  ;; Note reliance of spawn on the way this is implemented
  (zerop (num-running-threads c)))
	 
	 

(defun wake-up-cpp-threads (c cond-var)
  (incf (crlm-num-running-threads c) (num-waiting cond-var))
  (notify-all cond-var))

(defun am-holding-lock (c)
  "am-holding-lock CRLM.  Is the current process holding the crlm lock?"
  (eq *current-process* (process-lock-locker (lock c))))
      

(defun lookup-process (c &optional (p *current-process*))
  "lookup-process CRLM &optional (PROCESS *CURRENT-PROCESS*).  Return 1) the thread id associated with PROCESS 2) the associated thread-state object"
  (multiple-value-bind (thread-id t-id-present)
      (gethash p (process-thread-ids c))
    (assert t-id-present nil "Process ~a not found in the thread ID table" p)
    (multiple-value-bind (ts ts-present)
	(gethash thread-id (thread-states c))
      (assert ts-present nil "Thread ID ~a not found in the thread-state table" thread-id)
      (values thread-id ts))))


(defun initialize-thread (c proc func args)
  "initialize-thread CRLM PROCESS FUNC ARGS.  Initialize the process object PROCESS, so that once it is enabled it will begin by calling FUNC with arguments ARGS, then call die.  Then enable the process."
  (process-preset proc #'establish-bindings (list '*standard-output* ) (list *standard-output*) 
		  #'(lambda() 
		      (unwind-protect
			  (apply func args) 
			(die c))) nil)
  (process-enable proc))


(defun kill-all-threads (c)
  "kill-all-threads CRLM.  Kill all currently existing cpp threads.  The stacks of the threads are unwound correctly."
  
  (let ((killed-list
	 (loop
	     for p being each hash-key in (process-thread-ids c) using (hash-value id)
	     do (process-kill p) 
		(process-enable p) ;; necessary if p was at a wait
		
	     collect p)))

    ;; for the unix threading system - wait and make sure the threads are actually dead
    (process-unlock (lock c))
    (loop
	for p in killed-list
	do (process-wait "crlm-kill-whostate" (lambda (x) (not (member x *all-processes*))) p))
    (process-lock (lock c))
    )
  (setf (crlm-num-running-threads c) 0)
  (notify-all-observers c inform-part-prog-terminated))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessors for state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline get-env-state set-env-state))

(defun get-env-state (c)
  (js-env-state (joint-state c)))

(defun set-env-state (s c)
  (setf (js-env-state (joint-state c)) s))

(defun update-program-counter (t-state label)
  "update-program-counter T-STATE LABEL.  Update the program counter of thread state to be at LABEL.  Uses the value of *containing-subroutine-name* as the containing subroutine."
  (setf (ts-pc t-state)
    (make-pc *containing-subroutine-name* label)))

  
(defun at-env-action (c)
  "at-env-action C.  Is every effector assigned an action?"
  (loop 
      for a being each hash-value in (actions c)
      never (equal a 'action-unassigned)))

(defun current-effectors (c)
  "current-effectors C.  return a copy of the effector set of current environment state."
  (mapset 'list #'identity (env-user:current-effectors (env c))))

