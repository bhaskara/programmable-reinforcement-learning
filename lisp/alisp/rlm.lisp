;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/rlm.lisp
;; Contains the main code that defines what it means to execute an alisp program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rlm> ()
  (
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; the main components
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (env
    :type env-user:<env>
    :reader env
    :initarg :env)
   
   (partial-program
    :type <alisp-program>
    :reader part-prog
    :initarg :part-prog)
   
   (observers
    :reader observers
    :writer set-observers
    :initarg :observers)
   
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
    :accessor rlm-elapsed-steps)
   
   (elapsed-episodes
    :type fixnum
    :accessor rlm-elapsed-episodes)
   
   (joint-state
    :reader joint-state
    :writer set-joint-state
    :initform (make-joint-state)))
   
   
  (:documentation "Class that basically serves as a place to put all the relevant local state while running an ALisp program.  Required initargs are
:env - <env>
:part-prog - <part-prog>
:policy - <alisp-policy>
:observers - either of type <alisp-observer> or a list of <alisp-observer>s" ))
   

(defmethod initialize-instance :after ((r <rlm>) &rest args)
  (declare (ignore args))
  (let ((observers (observers r)))
    (unless (listp observers)
      (check-type observers <alisp-observer>)
      (set-observers (list observers) r))))


(define-condition rlm-last-step-reached ()
  ())

(define-condition env-terminated ()
  ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro notify-all-observers (rlm msg &rest args)
  (with-gensyms (obs)
    `(dolist (,obs (observers ,rlm))
       (,msg ,obs ,@args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running the partial program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run (rlm)
  "run RLM.  Run the partial program."
  
  ;; during this run, dynamically bind the *rlm* special variable to rlm
  ;; for debugging purposes, for now just setf *rlm*
  
  (setf *rlm* rlm
	(rlm-elapsed-steps rlm) 0
	(rlm-elapsed-episodes rlm) 0)
  (notify-all-observers rlm inform-start-execution)
  
  (handler-case
      
      ;; loop over episodes
      (loop
	;; this loop is exited when an rlm-last-step-reached condition
	;; is signalled
	(initialize-episode rlm)
	
	;; this form exits when the environment terminates
	;; or an rlm-last-step-reached error is signalled
	(handler-case
	    (start (part-prog rlm))
	  (env-terminated ()
	    (finish-episode rlm)
	    (check-last-episode rlm))))

    
    ;; inform observers that execution is done if necessary
    ((or rlm-last-step-reached choose-to-abort) ()
      (notify-all-observers rlm inform-finish-execution))))


(defun initialize-episode (rlm)
  (env-user:reset (env rlm))
  (let ((s (env-user:get-state (env rlm))))
    (set-joint-state
      (make-joint-state :env-state s :global nil
			:next-frame nil :pc nil
			:stack (list (make-frame 'top)) :choices nil)
      rlm)
    (notify-all-observers rlm inform-start-episode s)))

(defun check-last-episode (rlm)
  (let ((num-ep (num-episodes rlm))
	(ep (incf (rlm-elapsed-episodes rlm))))
    (when (and num-ep (>= ep num-ep))
      (signal 'rlm-last-step-reached))))


(defmethod finish-episode ((rlm <rlm>))
  
  ;; it's illegal for a partial program to terminate when the environment hasn't
  (assert (env-user:at-terminal-state (env rlm)) nil
    "Partial program terminated when environment was at non-terminal state ~a"
    (js-env-state (joint-state rlm)))
  
  ;; inform observers
  (notify-all-observers rlm inform-part-prog-terminated (clone (joint-state rlm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations called from partial programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro action (label action)
  "Macro action LABEL ACTION
ACTION evaluates to an action that is performed in the environment.
LABEL (not evaluated) is the label for this location in the program."  

  (with-gensyms (rlm omega act-var old-s reward new-s term num-steps new-frame cloned-state)
        
    ;; expanded code begins here
    `(let ((,rlm *rlm*)
	   (,omega (joint-state *rlm*))
	   (,act-var ,action)
	   (,new-frame (make-frame ',label)))
       
       
       ;; update program counter
       (update-program-counter ,rlm ',label)
       (setf (frame-label (first (js-stack ,omega))) ',label)
       
       ;; update joint state
       (setf (js-next-frame ,omega) ,new-frame
	     (js-choices ,omega) (list ,act-var)
	     (js-type ,omega) 'action)
       
       
       ;; copy state and inform observers
       (let ((,cloned-state (clone ,omega)))
	 (notify-all-observers ,rlm inform-alisp-step ,cloned-state ,act-var))
       
       (let ((,old-s (js-env-state ,omega)))
	 
	 ;; check that env hasn't terminated
	 (assert (not (env-user:at-terminal-state (env ,rlm))) nil 
	   "Tried to do action ~a but environment is at a terminal state ~a"
	   ,act-var ,old-s))

       ;; do the action
       (unwind-protect
       
	   ;; protected form
	   (multiple-value-bind (,reward ,new-s ,term)
	       (env-user:do-action (env ,rlm) ,act-var)
	 
	     ;; update the joint state
	     (setf (js-env-state ,omega) ,new-s
		   (js-choices ,omega) 'not-at-choice
		   (js-next-frame ,omega) 'not-at-choice
		   (js-type ,omega) 'action-exit)
	     (update-program-counter ,rlm ',(intern-compound-symbol label "-EXIT"))
	 
	     ;; inform observers
	     (notify-all-observers ,rlm inform-env-step ,act-var ,reward ,new-s ,term)
	 
	     ;; signal when env has terminated
	     (when ,term (error 'env-terminated)))
	 
	 
	 ;; cleanup forms
	 ;; update state
	 (setf (js-pc ,omega) 'internal
	       (js-choices ,omega) 'not-at-choice
	       (js-type ,omega) 'internal)

	 ;; update num elapsed steps, and signal when we've reached the last one
	 ;; if not, exit unwind-protect
	 (let ((,num-steps (num-steps ,rlm)))
	   (when (and ,num-steps (>= (incf (rlm-elapsed-steps ,rlm)) ,num-steps))
	     (error 'rlm-last-step-reached)))))))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessing env state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun env-state ()
  "get-state.  Return environment state.  The returned value should be treated as immutable, since the environment might be holding a reference to it."
  (env-user:get-state (env *rlm*)))

(defun env-has-terminated ()
  "env-has-terminated.  Return T iff env is at a terminal state."
  (env-user:at-terminal-state (env *rlm*)))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro call (&rest arg-list)
  "Macro call [LABEL] (FN-NAME &rest ARGS)

LABEL (not evaluated) is the label of this location in the partial program.  Optional, and if omitted, label defaults to FN-NAME.
FN-NAME (not evaluated) is the name of the function to be called.
ARGS (not evaluated) is a lambda list, in which some elements may be of the form (choose-arg CHOICE-LIST) where CHOICE-LIST is evaluated."
  (condlet
   (((= 1 (length arg-list)) (label (caar arg-list)) (fn-name (caar arg-list)) (args (cdar arg-list)))
    (t (label (car arg-list)) (fn-name (caadr arg-list)) (args (cdadr arg-list))))
   
   (let ((num-unspecified 0) ;; how many vars are unspecified
	 (choice-lists nil) ;; list of choices for each unspecified param
	 (unspecified (make-array 0 :adjustable t :fill-pointer 0)) ;; list where nth element is t if nth param unspecified
	 (unspecified-param-nums (make-array 0 :adjustable t :fill-pointer 0)) ;; list of numbers of unspecified params
	 (exit-label (intern-compound-symbol label "-EXIT")) ;; label of exit point of this call
	 (arg-vars ;; avoid multiple evaluation of arguments
	  (loop repeat (length args) collect (gensym))))

    
     ;; preprocessing to figure out at compile-time which parameters are specified
     (loop
	 for a in args
	 for i from 0
	 for unspec = (and (listp a) (eq (first a) 'choose-arg))
		     
	 do (vector-push-extend unspec unspecified)
	 when unspec
	 do (incf num-unspecified)
	    (push (second a) choice-lists)
	    (vector-push-extend i unspecified-param-nums))

    
     (with-gensyms (rlm new-frame ch c choice-list-vals 
			cloned-state omega param-names param-num
			var val)

       ;; this is where the expanded code begins
       `(let ((,rlm *rlm*)
	      (,new-frame (make-frame ',fn-name))
	      (,omega (joint-state *rlm*))
	      (,param-names (lookup-alisp-subroutine #',fn-name))
	      (,ch 'no-choice)
	      ,@(when choice-lists
		  `((,choice-list-vals (list ,@(reverse choice-lists)))))

	      ;; bind vars for args to avoid multiple evaluation	      
	      ,@(map 'list (lambda (a v u) `(,v ,(if u ''unspecified a))) args arg-vars unspecified)
	      )
	 
	  ;; update the program counter and state type
	  (update-program-counter ,rlm ',label)
	  (setf (frame-label (first (js-stack ,omega))) ',label)
	 
	  ;; add new frame
	  (setf (js-next-frame ,omega) ,new-frame)
	 
	  ;; add frame entries (some might be unspecified)
	  (mapc #'(lambda (,var ,val)
		     (set-frame-var-val ,new-frame ,var ,val nil))
		   ,param-names (list ,@arg-vars))
	 
	  ;; set the choice list and state typ
	  (setf (js-choices ,omega)
	    ,(cond ((= num-unspecified 1) `(first ,choice-list-vals))
		   ((> num-unspecified 0) choice-list-vals)
		   (t `'(no-choice)))
	    (js-type ,omega) 'call)
	 
	 
	  ;; make choice
	  (setf ,ch (choose-using-completion ,rlm ,omega))
	 
	  ;; inform observers 
	  (let ((,cloned-state (clone ,omega)))
	    (notify-all-observers ,rlm inform-alisp-step ,cloned-state ,ch))
	 
	  ;; fill in unspecified parameters into frame
	  ,(cond ((= num-unspecified 1)
		  `(set-frame-var-val ,new-frame (nth ,(aref unspecified-param-nums 0) ,param-names) ,ch t))
		 ((> num-unspecified 1)
		  `(map nil (lambda (,param-num ,c)
			      (set-frame-var-val ,new-frame (nth ,param-num ,param-names) ,c t))
			,unspecified-param-nums ,ch)))
	 
	 
	 
	  ;; move next frame to top of stack
	  (move-next-frame-to-stack ,omega)
	 
	  ;; fill in unspecified arguments of function call
	  ,(cond ((= num-unspecified 1)
		  `(setf ,(nth (aref unspecified-param-nums 0) arg-vars) ,ch))
		 ((> num-unspecified 1)
		  `(setf
		       ,@(loop
			     for i across unspecified-param-nums
			     collect (nth i arg-vars)
			     collect `(nth ,i ,ch)))))

	  (unwind-protect 
		
	      (handler-case
		  ;; do the function call (and return this as the final return value)
		  (,fn-name ,@arg-vars)
		  
		((or rlm-last-step-reached env-terminated) (,c)
		  (error ,c)))
	   
	    ;; cleanup forms
	    ;; pop stack, set pc to exit point of this choice
	    (pop (js-stack ,omega))
	    (update-program-counter ,rlm ',exit-label)
	    (setf (frame-label (first (js-stack ,omega))) ',exit-label)
	    (setf (js-choices ,omega) '(no-choice)
		  (js-type ,omega) 'call-exit)

	    ;; notify observers
	    (let ((,cloned-state (clone ,omega)))
	      (notify-all-observers ,rlm inform-end-choice-block ,cloned-state))
	   
	    ;; update state again and exit the unwind-protect
	    (setf (js-pc ,omega) 'internal
		  (js-choices ,omega) 'not-at-choice
		  (js-type ,omega) 'internal)
	    )
	  )))))
    
    

(defmacro with-choice (label (var choices) &body body)
  "with-choice LABEL (VAR CHOICES) &body BODY
LABEL (not evaluated) - label of this choice
VAR (not evaluated) - symbol that names the choice variable
CHOICES (evaluated) - set of choices
BODY (not evaluated) - set of forms enclosed by implicit progn

Bind VAR to a value chosen from CHOICES by the completion, then execute BODY."

  (with-gensyms (rlm new-frame omega cloned-state c)
    ;; expanded code starts here
    `(let ((,rlm *rlm*)
	   (,new-frame (make-frame ',label))
	   (,omega (joint-state *rlm*)))
     
       ;; update the program counter
       (update-program-counter ,rlm ',label)
       (setf (frame-label (first (js-stack ,omega))) ',label)

       ;; add new frame
       (setf (js-next-frame ,omega) ,new-frame)
     
       ;; add a single entry to this frame for the choice variable
       (set-frame-var-val ,new-frame ',var 'unspecified nil)

       ;; set the choice list and type
       (setf (js-choices ,omega) ,choices
	     (js-type ,omega) 'with-choice)
       
       ;; make choice and binding
       (let ((,var (choose-using-completion ,rlm ,omega)))
	 
	 ;; notify observers
	 (let ((,cloned-state (clone ,omega)))
	   (notify-all-observers ,rlm inform-alisp-step ,cloned-state ,var))
	 
	 ;; fill in unspecified parameter into frame
	 (set-frame-var-val ,new-frame ',var ,var t)
       
	 ;; move next frame to top of stack
	 (move-next-frame-to-stack ,omega)
	 
	 
	 (unwind-protect
	     (handler-case
		 ;; execute body
		 (progn ,@body )
		 
		 
	       ;; if a choose-to-abort happened within body, make a 
	       ;; note and pass on the error
	       ((or rlm-last-step-reached env-terminated) (,c)
		 (error ,c)))
	     
	   ;; cleanup forms
	   ;; pop stack, set pc to exit point of this choice
	   (pop (js-stack ,omega))
	   ,(let ((exit-label (intern-compound-symbol label "-EXIT")))
	      `(progn
		 (update-program-counter ,rlm ',exit-label)
		 (setf (frame-label (first (js-stack ,omega))) ',exit-label)))
	   (setf (js-choices ,omega) '(no-choice)
		 (js-type ,omega) 'with-choice-exit)
	     
	     
	   ;; notify all observers choice block is done
	   (let ((,cloned-state (clone ,omega)))
	     (notify-all-observers ,rlm inform-end-choice-block ,cloned-state)
	     )
	   
	   ;; update state again and exit unwind protect
	   (setf (js-pc ,omega) 'internal
		 (js-choices ,omega) 'not-at-choice
		 (js-type ,omega) 'internal))
	   
	 ))))
       

(defmacro choose (label &rest choices)
  "macro choose LABEL &rest CHOICES
LABEL (not evaluated) : label for this choice point
CHOICES (not evaluated) : list, where either all choices are of the form (CHOICE-NAME CHOICE-FORMS) where CHOICE-NAME is not the symbol 'call, or all choices are of the form (CALL &rest ARGS) where ARGS would be the arguments to a CALL statement. except that the choose-arg construct may not be used for any of the arguments"
  
  (let ((choice-labels (map 'vector 
			 (lambda (x) 
			   (if (eq (first x) 'call) 
			       (caadr x)
			     (car x))) 
			 choices))
	(forms (mapcar (lambda (x) (if (eq (first x) 'call) (second x) `(progn ,@(rest x)))) choices)))
    
    (with-gensyms (rlm new-frame omega choice cloned-state c)
      
      ;; expanded code starts here
      `(let ((,rlm *rlm*)
	     (,omega (joint-state *rlm*))
	     (,new-frame (make-frame ',label)))
	 
	 ;; update program counter
	 (update-program-counter ,rlm ',label)
	 (setf (frame-label (first (js-stack ,omega))) ',label)
	 
	 ;; add new frame
	 (setf (js-next-frame ,omega) ,new-frame)
	 
	 ;; add single entry to frame for choice-var
					;(set-frame-var-val ,new-frame 'choice-index 'unspecified nil)
	 
	 ;; set choice set, type
	 (setf (js-choices ,omega) ,choice-labels
	       (js-type ,omega) 'choose)
	 
	 ;; make choice and binding
	 (let ((,choice (choose-using-completion ,rlm ,omega)))

	   ;; inform observers
	   (let ((,cloned-state (clone ,omega)))
	     (notify-all-observers ,rlm inform-alisp-step ,cloned-state ,choice))
	 
	   ;; fill in unspecified parameter into frame
					;(set-frame-var-val ,new-frame 'choice-index ,choice-index t)
	   
	   ;; rename frame with label of the choice
	   (setf (frame-name ,new-frame) ,choice)
	 
	   ;; move next frame to top of stack
	   (move-next-frame-to-stack ,omega)

	   
	   (unwind-protect
	       (handler-case
		   ;; evaluate chosen form
		   (case ,choice
		     ,@(loop
			   for f in forms
			   for i across choice-labels
			   collect `(,i ,f)))
		 ((or rlm-last-step-reached env-terminated) (,c)
		   (error ,c)))

	     ;; cleanup forms
	     ;; pop stack, set pc to exit point of this choice
	     (pop (js-stack ,omega))
	     ,(let ((exit-label (intern-compound-symbol label "-EXIT")))
		`(progn
		   (update-program-counter ,rlm ',exit-label)
		   (setf (frame-label (first (js-stack ,omega))) ',exit-label)))
	     
	     (setf (js-choices ,omega) '(no-choice)
		   (js-type ,omega) 'choose-exit)
	       
	     (let ((,cloned-state (clone ,omega)))

	       ;; notify all observers choice block is done
	       (notify-all-observers ,rlm inform-end-choice-block ,cloned-state)
	       )

		   
	   
	     ;; update state again and exit unwind-protect
	     (setf (js-pc ,omega) 'internal
		   (js-choices ,omega) 'not-at-choice
		   (js-type ,omega) 'internal))
	   )))))
	 
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessing memory state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mem (item &optional (key nil key-supplied))
  "macro mem ITEM KEY
ITEM (evaluated) is the object to be stored
KEY (not evaluated) is the key to store it under.  If not provided, defaults to ITEM (not evaluated)

Stores the ITEM in the top frame in the choice stack"
  (when (not key-supplied)
    (assert (symbolp item) nil
      "Key was not supplied and ~a is not a symbol" item)
    (setf key item))
  
  (with-gensyms (rlm)
    ;; start of expanded code
    `(let ((,rlm *rlm*))
       (set-frame-var-val (top-frame (joint-state ,rlm)) ',key ,item))))

(defmacro gmem (item &optional (key nil key-supplied))
  "macro mem ITEM KEY
ITEM (evaluated) is the object to be stored
KEY (not evaluated) is the key to store it under.  If not provided, defaults to ITEM (not evaluated), assuming it's a symbol.

Stores ITEM in the global frame."
  (when (not key-supplied)
    (assert (symbolp item) nil
      "Key was not supplied and ~a is not a symbol" item)
    (setf key item))
  
  (with-gensyms (rlm)
    ;; start of expanded code
    `(let ((,rlm *rlm*))
       (set-frame-var-val (js-global (joint-state ,rlm)) ',key ,item))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun choose-using-completion (rlm omega)
  (handler-case
      (policy:make-choice (policy rlm) omega)
    (policy:unknown-state ()
      (sample-uniformly (js-choices omega)))))
  

(defun update-program-counter (rlm label)
  "update-env-state RLM LABEL.  Update the program counter of rlm.  LABEL is the label of the generalized choice point that the program is at.  Uses the value of *containing-subroutine-name* as the containing subroutine."
  (setf (js-pc (joint-state rlm)) 
    (make-program-counter :containing-subroutine *containing-subroutine-name* :label label)))



(defun lookup-alisp-subroutine (f)
  (let ((l (get-lambda-list f)))
    (assert (notany
	     (lambda (x) (member x '(&optional &key &rest &aux)))
	     l)
	() "ALisp subroutines cannot have optional, key, rest, or aux arguments.")
    l))
       




