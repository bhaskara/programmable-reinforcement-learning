;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; concurrent-alisp/calisp-state.lisp
;; Defines various components of the joint state of a Concurrent ALisp program 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package calisp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition non-choosing-thread ()
  ((id :initarg :id :reader get-id)
   (state :initarg :state :reader get-state))
  (:report (lambda (c s) (format s "Id ~a is not among choosing threads ~a" (get-id c) (js-choosing-thread-ids (get-state c))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joint state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (joint-state (:conc-name js-))
  env-state
  global
  thread-states
  choosing-thread-ids
  choices)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for joint states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((omega joint-state))
  (make-joint-state 
   :env-state (js-env-state omega)
   :global (clone (js-global omega))
   :thread-states (clone-thread-states (js-thread-states omega))
   :choosing-thread-ids (js-choosing-thread-ids omega)
   :choices (js-choices omega)))
   

(defmethod canonicalize ((omega joint-state))
  (list (canonicalize (js-env-state omega))
	(canonicalize (js-global omega))
	(canonicalize-thread-states (js-thread-states omega))))


(defun choice-set (omega choosing-thread-ids)
  "choice-set OMEGA THREADS.  The set of choices available in OMEGA to THREADS."
  (let ((t-states (js-thread-states omega)))
    (make-instance 'prod-set:<prod-set>
      :sets (mapcar #'(lambda (id) (ts-choices (gethash id t-states)))
		    choosing-thread-ids)
      :inst-acc (inst-vars:make-alist-accessors choosing-thread-ids #'equal))))

(defun js-thread (omega id)
  "js-thread OMEGA THREAD-ID"
  (gethash id (js-thread-states omega)))

(defun js-thread-label (omega id)
  "js-thread-label JOINT-STATE THREAD-ID."
  (ts-label (js-thread omega id)))

(defun js-thread-numbers (omega ids)
  "js-thread-number OMEGA IDS 
IDS is a designator for a list of thread ids. 

Returns list of positions of each id in list of choosing threads."
  
  (let ((choosing-ids (js-choosing-thread-ids omega)))
    (mapcar (lambda (id) 
	      (aif
	       (position id choosing-ids :test #'equal)
	       it
	       (error 'non-choosing-thread :state omega :id id)))
	    (designated-list ids))))
  

(defun threads-in-subtask (omega name)
  "threads-in-subtask OMEGA NAME.  Return a list of the ids of all threads whose stack contains a frame named NAME."
  (hash-table-select 
   (js-thread-states omega)
   (lambda (ts)
     (some (lambda (frame) (eq (frame-name frame) name)) 
	   (ts-stack ts)))))

(defun threads-at-label (omega label)
  "threads-at-label OMEGA LABEL.  Return a list of ids of threads whose program counter label is LABEL."
  (hash-table-select
   (js-thread-states omega)
   (lambda (ts)
     (eq label (ts-label ts)))))


(defun choosing-threads-at-label (omega label)
  "threads-at-label OMEGA LABEL.  Return a list of ids of threads whose program counter label is LABEL."
  (hash-table-select
   (js-thread-states omega)
   (lambda (ts)
     (and 
      (eq label (ts-label ts))
      (choosing ts)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (thread-state (:conc-name ts-))
  pc
  next-frame
  stack
  choices
  effectors
  (type nil "Can be 'choose, 'action, 'call, 'with-choice, any of these with -exit added on to the end, or internal")
  (status nil "Can be choosing, holding, acting, waiting-to-act, or running"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions on thread states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ts-label (ts)
  (program-counter-label (ts-pc ts)))

(defun ts-stack-depth (ts)
  (length (ts-stack ts)))

(defun make-thread-state-internal (ts)
  (setf (ts-pc ts) (make-pc nil 'internal)
	(ts-next-frame ts) nil
	(ts-choices ts) 'not-choosing
	(ts-type ts) 'internal
	(ts-status ts) 'running))

(defun clone-thread-states (t-states)
  (copy-hash-table t-states :value-copy-fn #'clone-thread-state))

(defun clone-thread-state (t-state)
  (make-thread-state :pc (ts-pc t-state) :next-frame (clone (ts-next-frame t-state))
		     :stack (clone (ts-stack t-state)) :choices (clone (ts-choices t-state))
		     :type (ts-type t-state) :status (ts-status t-state)
		     :effectors (clone (ts-effectors t-state))))


(defun canonicalize-thread-states (t-states)
  (loop 
      for k being each hash-key in t-states using (hash-value v)
      collect (cons k (canonicalize-thread-state v))))

;; TODO this isn't quite right
(defun canonicalize-thread-state (ts)
  (list (ts-pc ts) (ts-next-frame ts) (ts-stack ts) (canonicalize (ts-effectors ts))
	(ts-type ts) (ts-status ts)))




(defun move-next-frame-to-stack (ts)
  "move-next-frame-to-stack TS.  Moves next-frame of TS (which must be non-nil) to the top of the stack, and sets the next-frame field to nil."
  (let ((next-frame (ts-next-frame ts)))
    (assert next-frame nil "Attempted to move nil frame to stack")
    (push next-frame (ts-stack ts)))
  (setf (ts-next-frame ts) nil))


(defun choosing (ts)
  (eq (ts-status ts) 'choosing))

(defun at-label (ts label)
  (eq (ts-label ts) label)) 

(defun some-frame-at-label (ts label)
  "some-frame-at-label THREAD-STATE LABEL
Is some frame on the stack of THREAD-STATE at a location called LABEL?"
  (some #'(lambda (f) (eq (frame-label f) label)) (ts-stack ts)))

(defun some-frame-has-name (ts name)
  "some-frame-has-name THREAD-STATE NAME
Does some frame on the stack of THREAD-STATE have name equal to NAME?"
  (some #'(lambda (f) (equal (frame-name f) name)) (ts-stack ts)))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (frame (:constructor make-frame (name &optional (label nil) (entries nil)))
	    (:type list))
  name
  label
  (entries nil))



(defun set-frame-var-val (frame var val &optional (already-exists nil exists-supplied))
  "set-frame-var-val FRAME VAR VAL &optional ALREADY-EXISTS.
Set value of VAR in FRAME to VAL.  If ALREADY-EXISTS is supplied, then the existence or nonexistence of an entry for the given VAR is verified in advance and an assertion happens if the expectations fail."

  (let* ((entries (frame-entries frame))
	 (entry (assoc var (frame-entries frame))))
    (when exists-supplied
      (if already-exists
	  (assert entry nil "Contrary to expectations, entry for ~a does not exist in entries ~a of frame ~a"
		  var entries frame)
	(assert (not entry) nil "Contrary to expectations, entry ~a does exist in entries ~a of frame ~a"
		entry entries frame)))
    
    (if entry
	(setf (cdr entry) val)
      (push (cons var val) (frame-entries frame)))))

(defun get-frame-var-val (frame var)
  "get-frame-var-val FRAME VAR.  Return value of VAR in FRAME.  VAR must already exist."
  (let ((entries (frame-entries frame))
	(entry (assoc var (frame-entries frame))))
    (assert entry () "Entry for ~a does not exist among entries ~a of frame ~a" var entries frame)
    (cdr entry)))

;; frames are just lists, so same, clone, and canonicalize already defined

(defun stack-var-val (ts var-name)
  "stack-var-val THREAD-STATE VAR-NAME
If some stack frame contains a variable with this name, return its value, and true as a secondary value.  Otherwise, return nil and nil."
  
  (dolist (f (ts-stack ts))
    (dolist (b (frame-entries f))
      (when (eq (car b) var-name)
	(return-from stack-var-val (values (cdr b) t)))))
  (values nil nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (program-counter (:type list) (:constructor make-pc (containing-subroutine label)))
  containing-subroutine
  label)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pprint-calisp-state (str omega)
  (pprint-logical-block (str nil)
    (format str "Concurrent ALisp state")
    (format str "~2I~:@_Environment state~4I~:@_~W" (js-env-state omega))
    (format str "~2I~:@_Global memory ~/pprint-fill/" (frame-entries (js-global omega)))
    (format str "~2I~:@_Thread states~4I~:@_~/calisp::pprint-thread-states/" (hash-to-alist (js-thread-states omega)))
    (format str "~2I~:@_Choosing threads ~W" (js-choosing-thread-ids omega))
    ))

(set-pprint-dispatch 'joint-state #'pprint-calisp-state)

(defun pprint-thread-states (str t-states &rest args)
  (pprint-logical-block (str t-states)
    (loop
      (pprint-exit-if-list-exhausted)
      (dbind (id . ts) (pprint-pop)
	(format str "~:@_Thread ~a~2I~:@_~W~0I" id ts)))))

(defun pprint-thread-state (str ts)
  (pprint-logical-block (str nil)
    (flet ((print-pair (x y &optional (newline t))
	     (when newline
	       (pprint-newline :mandatory str))
	     (format str "~A:~2,20:@T~A" x (if (symbolp y) (symbol-name y) y))))
      (print-pair "Program counter" (program-counter-label (ts-pc ts)) nil)
      (print-pair "Stack" (ts-stack ts))
      (print-pair "Type" (ts-type ts))
      (print-pair "Status" (ts-status ts)))))

(set-pprint-dispatch 'thread-state #'pprint-thread-state)



(in-package cl-user) 