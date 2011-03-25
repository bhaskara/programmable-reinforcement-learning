(defpackage threadwise-decomposed-q
  (:documentation "Package threadwise-decomposed-q (thread-dec-q)

Threadwised decomposed Q-learning algorithm for concurrent ALisp.

Types
-----
<threadwised-decomposed-q-learner>
")
  
  (:export
   <threadwise-decomposed-q-learner>)
  (:nicknames thread-dec-q)
  (:use
   cl
   utils
   set
   calisp-obs
   policy
   dec-q-fn)
  )


(in-package thread-dec-q)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <threadwise-decomposed-q-learner> (<calisp-learning-algorithm> <q-learning-algorithm>)
  ((prev-omega :accessor prev-omega)
   (prev-u :accessor prev-u)
   (prev-s :accessor prev-s)
   (rewards :type list :accessor rewards)
   (reward-decomposer :type function :reader reward-decomposer :initarg :reward-decomposer)
   (q-function :type <decomposed-q-function>
	       :initarg :q-function :reader q-fn :writer set-q-fn)
   (initq :accessor initq)
   (root-thread-id :initarg :root-thread-id :initform 'calisp-prog:root-thread :reader root-thread-id)
   (discount :type float :initform 1.0 :reader discount :initarg :discount)
   (total-discount :accessor total-discount :type float)
   (newly-spawned-threads :accessor newly-spawned-threads :type list)
   (learning-rate :reader lrate :initarg :lrate :initform .02))
  (:documentation "Class <threadwise-decomposed-q-learner> (<calisp-learning-algorithm> <q-learning-algorithm>)

Initargs
:q-function - the initial q-function
:discount - the discount factor.  Defaults to 1.0
:learning-rate - the learning rate.  Of type [learning-rate].  Defaults to .02.
:reward-decomposer - A function that takes in five arguments : omega, s, a, r, s2, and returns a reward decomposition, i.e., an association list from the threads of omega to real-valued rewards which add up to r.
:root-thread-id - The root thread id.  Optional and defaults to 'calisp-prog:root-thread
"))


(defmethod initialize-instance :after ((alg <threadwise-decomposed-q-learner>) &rest args &key q-function)
  (declare (ignore args))
  (setf (initq alg) (clone q-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; responses to messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-episode ((alg <threadwise-decomposed-q-learner>) s)
  (setf (prev-s alg) s
	(rewards alg) nil
	(newly-spawned-threads alg) nil)
  (unbind-slot alg 'prev-omega)
  (unbind-slot alg 'prev-u)
  (mapping:set-value (rewards alg) (root-thread-id alg) 0.0 nil))


(defun inform-finish-episode (alg)
  (when (slot-boundp alg 'prev-omega)
    (perform-backups alg t nil nil)))


(defmethod inform-calisp-step ((alg <threadwise-decomposed-q-learner>) omega u)
  ;; If this is not the first joint state we have encountered this episode, do an update
  (when (slot-boundp alg 'prev-omega)
    (perform-backups alg nil omega u))
  
  ;; Remember this state and choice
  (setf (prev-omega alg) omega
	(prev-u alg) u
	(total-discount alg) 1.0
	(newly-spawned-threads alg) nil)
  (dolist (entry (rewards alg))
    (setf (cdr entry) 0.0)))


(defmethod inform-env-step ((alg <threadwise-decomposed-q-learner>) a r s2 term)
  (when (slot-boundp alg 'prev-omega)
    (let ((rewards (funcall (reward-decomposer alg) (prev-omega alg) (prev-s alg) a r s2)))
      (assert (= r (sum-over rewards #'cdr)) () 
	"Rewards ~a don't add up to ~a in reward decomposition for doing ~a in ~a and getting to ~a"
	rewards r a (prev-s alg) s2)
      (let ((reward-accs (rewards alg)))
	(dolist (entry rewards)
	  (let ((acc-entry 
		 (check-not-null (assoc (car entry) reward-accs :test #'equal)
				 "The reward accumulator entry for ~a in ~a" (car entry) reward-accs)))
	    (incf (cdr acc-entry) (cdr entry))
	    (multf (total-discount alg) (discount alg))))))
    
    (when term (inform-finish-episode alg)))
  (setf (prev-s alg) s2))


(defmethod inform-spawn-thread ((alg <threadwise-decomposed-q-learner>) id current-thread effs)
  (declare (ignore effs))
  (acons id current-thread (newly-spawned-threads alg))
  (mapping:set-value (rewards alg) id 0.0 nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; knowledge state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knowledge-state ((alg <threadwise-decomposed-q-learner>) &optional (fresh t))
  (let ((q (q-fn alg)))
    (if fresh (clone q) q)))


(defmethod get-q-fn ((alg <threadwise-decomposed-q-learner>) ks)
  ks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <threadwise-decomposed-q-learner>) )
  (set-q-fn (clone (initq alg)) alg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perform-backups (alg terminal? omega u)
  
  (with-slots (prev-omega prev-u total-discount q-function 
	       rewards newly-spawned-threads) alg
    (let ((new-comps (when omega (funcall (component-fn q-function) omega))))
      (do-elements (id (funcall (component-fn q-function) prev-omega))
	
	;; the update will use this thread and all other threads spawned by it on the last step
	(let ((relevant-threads (list* id (filter 'list newly-spawned-threads
						  #'(lambda (entry) (equal (car entry) id))))))
	  (let ((target
		 
		 (sum-over relevant-threads
			   #'(lambda (i)
			       (+ 
				   
				;; any accumulated reward
				(or (cdr (assoc i rewards)) 0.0)
				   
				;; the q-value at the dest state, if necessary
				(if (and (not terminal?)
					 (member i new-comps :test #'equal))
				    (* total-discount
				       (handler-case
					   (evaluate-component q-function i omega u)
					 (q-fn:unknown-state-action () 0)))
				  0))))))
	    
	    (update-component
	     q-function id prev-omega prev-u target 
	     (lrate:get-rate (lrate alg) (cons prev-omega prev-u)))
	    (debug-msg alg "~&Did backup of component ~a towards target ~a.~&Weights are now ~a"
		       id target (crlq:weights q-function))))))))


