(defpackage temporally-decomposed-q
  (:documentation "Package temporally-decomposed-q (temp-dec-q)

Temporally (and threadwise) decomposed Q-learning algorithm for concurrent ALisp.

Types
-----
<temporally-decomposed-q-learner>")
  (:export
   <temporally-decomposed-q-learner>)
  (:nicknames temp-dec-q)
  (:use
   cl
   utils
   mapping
   set
   circ-vec
   calisp-q-fn
   calisp-obs
   policy))

(in-package temp-dec-q)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def and accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <temporally-decomposed-q-learner> (<calisp-learning-algorithm> <q-learning-algorithm>)
  ((prev-omega :accessor prev-omega)
   (prev-choice :accessor prev-choice)
   (prev-s :accessor prev-s)
   (env-terminated? :accessor env-terminated?)

   (choice-stacks :type list :accessor choice-stacks)
   (exit-points :type list :accessor exit-points)
   (newly-spawned :type list :accessor newly-spawned)
   
   (q-function :type dec-q-fn:<decomposed-q-function> :reader q-fn :writer set-q-fn :initarg :q-function)
   (init-q-function :type dec-q-fn:<decomposed-q-function> :accessor initq)
   
   (root-thread-id :initarg :root-thread-id :initform 'calisp-prog:root-thread :reader root-thread-id)
   
   (reward-decomposer :type function :reader reward-decomposer :initarg :reward-decomposer)
   (learning-rate :reader lrate :initarg :lrate :initform .02)
   )
  (:documentation "Class <temporally-decomposed-q-learner> (<calisp-learning-algorithm> <q-learning-algorithm>)
Represents a temporally (and threadwise) decomposed Q-learning algorithm for Concurrent ALisp programs.

Initargs
:reward-decomposer
:lrate - learning rate (.02 by default)
:q-function - the initial q-function
"))

(defun thread-ids (ql)
  (mapcar #'car (choice-stacks ql)))


(defmethod initialize-instance :after ((alg <temporally-decomposed-q-learner>) &rest args &key q-function)
  (declare (ignore args))
  (setf (initq alg) (clone q-function)))

(defmethod reset ((alg <temporally-decomposed-q-learner>))
  (set-q-fn (clone (initq alg)) alg))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *q-comps* '(qr qc qe))
(defvar *debug-mode* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choice-stack items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct csi
  "Structure csi (choice-stack item).  Contains bookkeeping information about previous choices made for which backups will be done for a threadwise Q-function."
  
  ;; the last state that was seen at this level, or nil if no state was seen
  omega
  
  ;; the choice that was made
  choice

  ;; the label of this thread at omega
  (label nil)
  
  ;; accumulated reward pertaining to Qr component
  (r-acc 0)

  ;; accumulated reward pertaining to Qc component
  (c-acc 0)

  ;; accumulated reward pertaining to Qe component
  (e-acc 0)
  
  ;; there should eventually be a discount term here
  
  ;; learning rate when choice was made
  lrate
  
  ;; type of statement
  type
  
  ;; actions done since then?
  (action-done? 'n/a)
  
  ;; thread being stepped?
  (stepped? 'n/a)
  )




(defun no-actions-done (c)
  (assert (eq (csi-type c) 'action))
  (not (csi-action-done? c)))

(defun set-no-actions-done (c v)
  (assert (eq (csi-type c) 'action))
  (setf (csi-action-done? c) (not v)))

(defsetf no-actions-done set-no-actions-done)

(defun stepped (c)
  (assert (member (csi-type c) '(spawn choice)))
  (csi-stepped? c))

(defun set-stepped (c v)
  (assert (member (csi-type c) '(spawn choice)))
  (setf (csi-stepped? c) v))

(defsetf stepped set-stepped)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state history (debugging)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *state-hist*)

(defun state-num (omega)
  (let ((v (get-flat-vector *state-hist*)))
    (or (position omega v) 'no-state-visited)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo make function

(defmacro backup (alg omega u thread comp target)
  `(progn
     (pprint-backup ,alg ,omega ,u ,thread ,comp ,target)
     (dec-q-fn:update-component 
      (q-fn ,alg) (list ,thread ,comp) ,omega ,u ,target
      (lrate:get-rate (lrate ,alg) (cons ,omega ,u)))))
						

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; responses to messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod inform-start-episode ((alg <temporally-decomposed-q-learner>) s)
  (let ((root-thread-id (root-thread-id alg)))
    (setf (prev-s alg) s
	  (newly-spawned alg) nil
	  (choice-stacks alg) (list (cons root-thread-id (list (make-csi :omega nil))))
	  (env-terminated? alg) nil
	  (exit-points alg) (list (cons root-thread-id nil))))
  (unbind-slot alg 'prev-omega)
  (unbind-slot alg 'prev-choice)
  (setf *state-hist* (make-circular-vector 100)))


(defmethod inform-die-thread ((alg <temporally-decomposed-q-learner>) id)
  
  (let ((stack (evaluate (choice-stacks alg) id))
	(exit-pts (evaluate (exit-points alg) id)))
    
    (if (env-terminated? alg)
	
	;; The case where the env terminates, causing the partial program to terminate
	;; A. For each choice-stack item
	(progn
	  (while stack
	    (let ((c (pop stack)))
	      ;; if this is not the top stack item, do a qe backup if possible
	      (awhen (csi-omega c)
		(backup alg it (csi-choice c) id 'qe 0))
		  
	      ;; if it's the top stack item, do a qc backup if possible
	      (unless stack
		(awhen (csi-omega c)
		  (backup alg it (csi-choice c) id 'qc 0)))))
	    
	  ;; B. For each active exit point do a Qe backup
	  (dolist (c exit-pts)
	    (backup alg (csi-omega c) (csi-choice c) id 'qe (csi-e-acc c))))
    
      ;; The case where the thread terminates without the rest of the program terminating
      (progn
      
	(assert (= (length stack) 1))
      
	;; do backups for the top stack item
	(let ((omega (csi-omega (first stack)))
	      (u (csi-choice (first stack))))
	  
	  (when omega
	    (backup alg omega u id 'qc 0)
	    (backup alg omega u id 'qe 0)))
      
	;; do exit point backups
	(dolist (c exit-pts)
	  (backup alg (csi-omega c) (csi-choice c) id 'qe (csi-e-acc c))))))
    
  ;;(make-undefined (exit-points alg) id) t)
  )


(defmethod inform-arrive-choice-state ((alg <temporally-decomposed-q-learner>) omega)
  (pprint-status alg "CALisp step")
  (when (debug-str alg)
    (circular-push omega *state-hist*))
  
  (let ((stacks (choice-stacks alg))
	(best (handler-bind
		  ((q-fn:unknown-state-action #'q-fn:choose-randomly))
		(q-fn:best-choice (q-fn alg) omega))))
    
    ;; For each thread
    (dolist (thread (thread-ids alg))
	
      (let ((qr (comp-q-value alg omega best thread 'qr))
	    (qc (comp-q-value alg omega best thread 'qc))
	    (qe (comp-q-value alg omega best thread 'qe))
	    (stack (evaluate stacks thread))
	    (exit-pts (evaluate (exit-points alg) thread)))
	
	(declare (ignorable qr qc qe))
	;; TODO remove this
	  
	(let ((current (first stack)))
	  (aif (csi-omega current)
	      (let ((prev-u (csi-choice current)))
	      
		;; if there's a previous state at this level
		;; do a set of backups depending on the type of statement
		;; and whether or not the thread has been stepped since then
		(ecase (csi-type current)
		
		  (action 
		   (if (no-actions-done current)
		       (progn
			 (backup alg it prev-u thread 'qr qr)
			 (backup alg it prev-u thread 'qc qc)
			 (backup alg it prev-u thread 'qe qe))
		     (progn
		       (backup alg it prev-u thread 'qr (csi-r-acc current))
		       (backup alg it prev-u thread 'qc (+ qr qc (csi-c-acc current)))
		       (backup alg it prev-u thread 'qe qe))))
		
		  ((choice spawn)
		   (if (stepped current)
		       (progn
			 (backup alg it prev-u thread 'qc 
				 (+ qr qc (csi-c-acc current)))
			 (backup alg it prev-u thread 'qe qe))
		     (progn
		       (backup alg it prev-u thread 'qr (+ qr (csi-r-acc current)))
		       (backup alg it prev-u thread 'qc qc)
		       (backup alg it prev-u thread 'qe qe)))))
		
		;; qe backups for all pending exit points
		(dolist (c exit-pts)
		  (backup alg (csi-omega c) (csi-choice c) thread 'qe
			  (+ qr qc qe (csi-e-acc c))))
		(setf (evaluate (exit-points alg) thread) nil))
	      
	    ;; otherwise, if there's a higher level, do a qr backup
	    (aif (second stack)
		(backup alg (csi-omega it) (csi-choice it) thread 
			'qr (+ qr qc (csi-r-acc it)))
		
	      ;; otherwise, if this is a newly spawned thread, do a backup to the parent
	      (let ((parent (evaluate-mv (newly-spawned alg) thread)))
		(awhen parent
		  (let ((c (first (evaluate stacks it))))
		    (backup alg (csi-omega c) (csi-choice c) parent 'qr (+ qr qc qe))))))))))))
	  
    
  


(defmethod inform-calisp-step ((alg <temporally-decomposed-q-learner>) omega u)
  
  
  
  (setf (prev-omega alg) omega
	(newly-spawned alg) nil)
  
  (let ((stacks (choice-stacks alg)))
  
    ;; for each thread
    (do-entries (id state (js-thread-states omega))
      (let ((stack (evaluate stacks id)))
      
	;; update the csi currently at the top of the stack
	(let ((current (first stack)))
	  (setf (csi-omega current) omega
		(csi-choice current) u
		(csi-lrate current) (lrate:get-rate (lrate alg) omega)
		(csi-r-acc current) 0
		(csi-c-acc current) 0
		(csi-e-acc current) 0
		
		(csi-label current)
		(js-thread-label omega id)
		
		(csi-type current)
		(ecase (ts-type state)
		  ((choose call with-choice) 'choice)
		  (spawn 'spawn)
		  (action 'action)))

	  (let ((is-choosing (eq (ts-status state) 'choosing)))
	    (if (eq (csi-type current) 'action)
		(set-no-actions-done current t)
	      (set-stepped current is-choosing))
	  
	    ;; add a new csi if necessary
	    (when (and is-choosing (eq (csi-type current) 'choice))
	      (push (make-csi :omega nil) (evaluate stacks id)))))))
    )
  
  )

(defmethod inform-spawn-thread ((alg <temporally-decomposed-q-learner>) id current-thread effs)
  (declare (ignore effs))
  (set-value (choice-stacks alg) id 
	     (list (make-csi :omega nil))
	     nil)
  (set-value (exit-points alg) id nil nil)
  (set-value (newly-spawned alg) id current-thread nil))
	     

(defmethod inform-end-choice-block ((alg <temporally-decomposed-q-learner>) thread omega)
  (declare (ignore omega))
  
  (let ((current (pop (evaluate (choice-stacks alg) thread)))
	(parent (first (evaluate (choice-stacks alg) thread))))
    
    (aif (csi-omega current)
	
	;; if at least one state was visited at this level
	(let ((u (csi-choice current)))
	  (backup alg it u thread 'qc (csi-c-acc current)) ; do a qc backup
	  (push current	(evaluate (exit-points alg) thread))) ;; add to exit point list
	
      ;; otherwise, do a qr backup for parent if necessary
      (when parent
	(backup alg (csi-omega parent) (csi-choice parent) thread 'qr
		(csi-r-acc parent))))

    (unless parent
      (make-undefined (choice-stacks alg) thread t)
      (dolist (c (evaluate (exit-points alg) thread))
	(backup alg (csi-omega c) (csi-choice c) thread 'qe (csi-e-acc c)))
      (make-undefined (exit-points alg) thread t))))
	     


(defmethod inform-env-step ((alg <temporally-decomposed-q-learner>) a r s2 term)
  
  (setf (env-terminated? alg) term)
  
  ;; if no choice state has been seen yet, no reward decomp is possible
  (when (slot-boundp alg 'prev-omega)
    (let ((rewards (funcall (reward-decomposer alg) (prev-omega alg) (prev-s alg) a r s2)))
      (with-slots (choice-stacks newly-spawned exit-points) alg

	;; verify rewards add up
	(assert (= r (sum-over rewards #'cdr)) ()
	  "Rewards ~a don't add up to ~a in reward decomposition for doing ~a in ~a and getting to ~a"
	  rewards r a (prev-s alg) s2)

	;; iterate over reward-receiving threads
	(dolist (id (thread-ids alg))
	  (let ((r (or (evaluate-mv rewards id) 0)))
	      
	    ;; is the thread newly spawned?
	    (aif (evaluate-mv newly-spawned id)

		;; If yes, look up the csi corresponding to the parent thread
		;; and increment the reward accumulator
		(incf (csi-r-acc (first (evaluate choice-stacks it))) r)

	      ;; Otherwise, the thread is not newly spawned.
	      ;; Get its choice stack and relevant entries
	      (let ((stack (evaluate choice-stacks id))
		    (thread-exit-pts (evaluate-mv exit-points id)))
		(let ((current (first stack))
		      (parent (second stack)))

		  (if (csi-omega current)
			
		      ;; If some state has been seen at this level,
		      ;; Accumulate reward for it and for all active exit points
		      (progn
			(acc-last-state-at-level current r)
			(dolist (exit-pt thread-exit-pts)
			  (incf (csi-e-acc exit-pt) r)))
		      
		    ;; otherwise, no state has been seen yet at this level
		    ;; so increment the parent's r-acc
		    (progn
		      (assert (and parent (eq (csi-type parent) 'choice) 
				   (stepped parent) (null thread-exit-pts)))
		      (incf (csi-r-acc parent) r)))))))))))
  
  (setf (prev-s alg) s2)
  )

(defun acc-last-state-at-level (current r)
  "helper function for inform-env-step that increments the appropriate reward accumulator in current, the csi at the lowest level of the thread, by r."
  			  
  (ecase (csi-type current)

    ;; if thread was at an action statement at that state
    ;; increment the r-acc if this is the first action since that state
    ;; or the c-acc otherwise
    (action (if (no-actions-done current)
		(progn
		  (setf (no-actions-done current) nil)
		  (incf (csi-r-acc current) r))
	      (incf (csi-c-acc current) r)))
			  
    ;; otherwise, if the thread was stepped, increment its
    ;; c-acc, and if not, increment its r-acc
    ((spawn choice) (if (stepped current)
			(incf (csi-c-acc current) r)
		      (incf (csi-r-acc current) r)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; knowledge state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knowledge-state ((alg <temporally-decomposed-q-learner>) &optional (fresh t))
  (let ((q (q-fn alg)))
    (if fresh (clone q) q)))


(defmethod get-q-fn ((alg <temporally-decomposed-q-learner>) ks)
  ks)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; q-evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun thread-q-value (alg omega u thread)
  (sum-over *q-comps* 
	    #'(lambda (comp)
		(comp-q-value alg omega u thread comp))))


(defun comp-q-value (alg omega u thread comp)
  (handler-bind
      ((q-fn:unknown-state-action
	#'(lambda (c)
	    (declare (ignore c))
	    (use-value 0))))
    (dec-q-fn:evaluate-component (q-fn alg) (list thread comp) omega u)))
 
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pprint functions for debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-backup (alg omega u thread comp target)
  (let ((str (debug-str alg)))
    (when str
      (terpri str)
      (pprint-logical-block (str nil)
	(flet ((print-pair (x y)
		 (pprint-tabulated-pair str x y 20 t)))
	  (format str "~:@_Performing backup~2I")
	  (print-pair "State" (state-num omega))
	  (print-pair "Choice" u)
	  (print-pair "Component" (list thread comp))
	  (print-pair "Target" target))))))
    

(defun pprint-status (alg init-msg)
  (let ((str (debug-str alg)))
    (when str
      (terpri str)
      (let ((ids (thread-ids alg)))
	(pprint-logical-block (str ids)
	  (format str "~A.  Algorithm status:" init-msg)
	
	  (pprint-indent :block 4 str)
	  (loop
	    (pprint-exit-if-list-exhausted)
	    (pprint-newline :mandatory str)
	    (pprint-thread-info (pprint-pop) alg str)))))))

(defun pprint-thread-info (id alg &optional (str t))
  (pprint-logical-block (str nil)
    (format str "Thread ~a" id)
    (awhen (evaluate-mv (newly-spawned alg) id)
      (pprint-indent :block 2 str)
      (format str "~:@_Newly spawned with parent ~W" it))
    (do-elements (c (reverse (evaluate (choice-stacks alg) id)) nil i)
      (pprint-indent :block 2 str)
      (format str "~:@_Choice stack item ~W" i)
      (pprint-indent :block 4 str)
      (pprint-newline :mandatory str)
      (pprint-csi c str))
    (do-elements (c (evaluate (exit-points alg) id) nil i)
      (pprint-indent :block 2 str)
      (format str "~:@_Active exit point ~W~4I~:@_" i)
      (pprint-csi c str))))
    

  

(defun pprint-csi (c &optional (str t))
  (pprint-logical-block (str nil)
    (flet ((print-pair (x y &optional (newline t))
	     (pprint-tabulated-pair str x y 20 newline)))
      (print-pair "State" (format nil "~a (~a)" (state-num (csi-omega c)) (csi-label c)) nil)
      (print-pair "Choice" (csi-choice c))
      (print-pair "Accumulators" (list (csi-r-acc c) (csi-c-acc c) (csi-e-acc c)))
      (print-pair "Type" (csi-type c))
      (print-pair "Action-done" (csi-action-done? c))
      (print-pair "Stepped?" (csi-stepped? c)))))