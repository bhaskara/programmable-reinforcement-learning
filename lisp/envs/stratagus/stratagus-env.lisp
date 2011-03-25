(in-package stratagus-env)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *socket* nil "The socket being used by the game.")
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class definition
;; constructor takes single required initarg :game-socket, which must be a socket connected to a 
;; running instance of the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <stratagus-env> (<fully-observable-env>)
  ((game-socket :type socket-stream-internet-active
		:accessor stratagus-env-game-socket
		:initform (make-socket :remote-host 'localhost :remote-port 4870)
		:reader game-socket)
   (num-steps :type fixnum
	      :documentation "Number of environment time steps (does not get reset when the environment state is reset"
	      :accessor num-steps
	      :initform 0)
   (world        :writer set-world
		 :reader world
                 :type array)
   (player-id    :writer set-pid
		 :reader pid
		 :type fixnum))
  (:documentation "<stratagus-env> class. Initargs
:game-socket - a socket to the stratagus game.  Defaults to one connected to port 4870 of localhost."))



;; after initializing, set value of state slot to be current game state
(defmethod initialize-instance :after ((e <stratagus-env>) &rest args)
  (setf *socket* (game-socket e))
  (set-state (get-game-state e) e)
  (set-pid (progn 
	     (format (game-socket e) "~a~%" *request-gameinfo-message*)
	     (force-output (game-socket e))
	     (gameinfo-player-id (read (game-socket e)))) e))
  ; (set-world (generate-map e) e))
;; i commented this out temporarily because it caused an error in tactical-fc-env

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; do-action
;; e : <stratagus-env>
;; act : joint action
;;
;; return 
;; - reward
;; - next state
;; - at terminal state?
(defmethod do-action ((e <stratagus-env>) act &aux (s (game-socket e)))
  (declare (ignore fresh))
  (check-type act list)

  ;; send all the unit actions
  (dolist (entry act)
    (destructuring-bind (id . a) entry
      (unless (equal a *noop*)
	(format s "~a ~a ~a~%" *command-message* id a)
;	(print (format nil "~a ~a ~a~%" *command-message* id a))
	(force-output s))))
;	(let ((response (read s)))
;	  (assert (equal *ok-message* (symbol-name response)) () 
;	    "Action ~a for effector ~a failed.  Response was ~a." a id response)))))
  
  (send e *trans-message*)
  
  ;; receive new state and return
  (let* ((new-state (get-game-state e))
	 (r (compute-reward e (get-state e) act new-state)))
    (set-state new-state e)
    (values r new-state (at-terminal-state e))))


;; reset
;; e : <stratagus-env>
;;
;; reset state.
;; subclasses might want to define :after methods for this to randomize the initial state
(defmethod reset ((e <stratagus-env>) &optional (reset-to-nonterminal-state t) &aux (s (game-socket e)))
  (assert reset-to-nonterminal-state)
  (clear-input s)
  (send e *reset-message* t)
  (set-state (get-game-state e) e))



(defgeneric send (e m &optional check-ok)
  (:documentation "send E M &optional (CHECK-OK nil).  Send message M over the socket to the stratagus environment and wait for, and return a response.  If CHECK-OK is t, then additionally verify that the response is *ok-message*.  Could hang if the connection is having issues and fails to respond.")
  (:method ((e <stratagus-env>) m &optional (check-ok nil))
	   (send (game-socket e) m check-ok))
  (:method ((s socket) m &optional (check-ok nil))
	   (declare (ignore check-ok))
	   (format s "~a~%" m)
	   (force-output s)))
;          (print (format nil "~a~%" m))))
;	   (let ((resp (read s)))
;	     (when check-ok
;	       (assert (equal (symbol-name resp) *ok-message*)))
;	     resp)))
      


(defmethod policy:prompt-for-choice (s choices (e <stratagus-env>))
  (declare (ignore choices s))
  
  (format t "~&Enter number of commands (or -1 to exit).")
  (let ((num-commands (read-from-string (read-line))))
    (when (eql num-commands -1)
      (error 'policy:choose-to-abort))
  
    (mapset 'list
	    (lambda (i)
	      (format t "~&Command ~a.~&Unit to command? " i)
	      (let ((id (read t)))
		(format t "~&Command string? ")
		(let ((cmd (read-line t)))
		  (cons id cmd))))
	    num-commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-game-state
;; e : <stratagus-env>
;;
;; receive the state from the stratagus game, parse and return it.
(defmethod get-game-state ((e <stratagus-env>) &aux (s (game-socket e)))
  
  ;; request state
  (format s "~a~%" *request-state-message*)
  (force-output s)
  (format s "~a~%" *request-map-message*)
  (force-output s)

  ;; flush all OKs
  (let ((a (read s)))
    (until (not (symbolp a))
;      (assert (equal (symbol-name a) *ok-message*) () "Action for effector failed.  Response was ~a." a)
      (setf a (read s)))

    ;; receive and parse state. 
    (multiple-value-bind (unit-state global-state)
        (loop
  	    with h = (make-hash-table :test #'eq)

            ;;; loop over alist and add each element to hashtable
	    for x in (first a)
	    do (setf (gethash (car x) h) (cdr x))
	     
	    finally (return (values h (second a))))
    
      (setf (global-state-map (second a)) (read s))
      (make-strat-es :units unit-state :global global-state))))

  

(defgeneric compute-reward (e s a s-new)
  (:documentation "compute-reward (E <STRATAGUS-ENV>) (S strat-env-state) ACT (S-NEW strat-env-state).  Reward function of the environment.")
  (:method ((e <stratagus-env>) s a s-new)
	   (declare (ignore s a s-new))
	   0))
	
	   

(defun manhattan-dist (l1 l2)
  (loop
      for x in l1
      for y in l2
      summing (abs (- x y))))	


(defmethod distance ((e <stratagus-env>) loc1 loc2 &optional (scale nil))
  "distance ENV LOC1 LOC2 &optional (SCALE nil).  For now, just return manhattan distance.  If SCALE is true, then divide by max distance"
  (/ (manhattan-dist loc1 loc2) (if scale (get-max-dist e) 1)))

(defmethod get-max-dist ((e <stratagus-env>))
  (let ((d (array-dimensions (world e))))
    (- (+ (first d) (second d)) 2)))

(in-package cl-user)