(defpackage alisp-hordq
  (:nicknames ahq)
  (:documentation "Package alisp-hordq.  Hierarchically optimal recursively decomposed Q-learning for ALisp.

Types
-----
<hordq>



Also see the alisp-user package for other operations on <alisp-learning-algorithm>s
")
  (:export
   <hordq>
   <rordq>
   <holyq>)
  
  (:use 
   common-lisp
   utils
   policy
   prob
   aed
   alisp-obs)
  (:import-from
   q-fn
   unknown-state-action
   choose-randomly
   sum-q-functions
   choices
   best-choice))
   

(in-package alisp-hordq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct csi
  "Structure type CSI (Choice-stack item).  Contains bookkeeping information regarding the last past joint choice at some level of the choice stack, for which backups may be done in the future."
  ;; the choice state we were at (or nil)
  omega 
  
  ;; the choice we made
  u 
  
  ;; have we actually visited any choice states at this level yet?
  (visited nil)
  
  ;; accumulated reward at lower levels since omega
  (r-acc 0)
  
  ;; accumulated reward at same level since omega
  (c-acc 0)
  
  ;; accumulated reward at higher levels since omega
  (e-acc 0)
  
  ;; omega's view of the discount factor of current time
  (discount 1.0)
  
  ;; learning rate when the choice was made
  lrate
  
  ;; has a qr-backup happened for omega?
  (qr-backup-flag nil)
)    
  
(defmethod print-object ((c csi) str)
  (format str "<CSI with omega = ~a~&u = ~a visited ~a qr-backup-flag ~a &discount ~a lrate ~a~&accumulators ~a>" 
	  (csi-omega c) (csi-u c) (csi-visited c) (csi-qr-backup-flag c) 
	  (csi-discount c) (csi-lrate c)
	  (list (csi-r-acc c) (csi-c-acc c) (csi-e-acc c))))



(defclass <hordq> (<alisp-learning-algorithm> <q-learning-algorithm>)
  ((choice-stack 
    :type list
    :reader choice-stack
    :initform nil
    :accessor hordq-choice-stack)
   (active-exit-points 
    :type list
    :reader active-exit-pts
    :accessor hordq-active-exit-pts
    :initform nil
    :documentation "Holds choices that we've exited, but whose Qe value has not yet been updated.")
   (discount
    :reader discount
    :initarg :discount
    :initform 1.0)
   (qr :reader qr :writer set-qr)
   (qc :reader qc :writer set-qc)
   (qe :reader qe :writer set-qe)
   (init-qr :reader init-qr :writer set-init-qr)
   (init-qc :reader init-qc :writer set-init-qc)
   (init-qe :reader init-qe :writer set-init-qe)
   (q :reader q :writer set-q)
   (learning-rate :reader lrate :initform .02
		  :initarg :lrate
		  :type lrate:<learning-rate>))
  (:documentation "Class <hordq>.  Does hierarchical q-learning, using the 3-part decomposition Q = Qr + Qc + Qe.

Initargs
:lrate - learning rate
:discount - discount factor
:hist-inc - how often to save hist
:debug-str - nil by default.  If non-nil, the state of the alg is logged to this stream
:features - if provided, this must be a list with three elements, namely the featurizers for Qr, Qc, and Qe.

"))

(defmethod initialize-instance :after ((alg <hordq>) &rest args &key (features nil))
  (declare (ignore args))
  (labels ((make-fn-approx ()
	     (make-instance 'fn-approx:<tabular-fn-approx> :test #'equalp))
	   (make-q-fn (feats)
	     (if feats
		 (make-instance 'alisp:<alisp-approx-q-function>
		   :featurizer feats 
		   :fn-approx (make-fn-approx))
	       (make-instance 'alisp:<alisp-approx-q-function>
		 :fn-approx (make-fn-approx)))))
    (let ((qr (make-q-fn (first features)))
	  (qc (make-q-fn (second features)))
	  (qe (make-q-fn (third features))))
      (set-qr qr alg)
      (set-qc qc alg)
      (set-qe qe alg)
      (set-q (sum-q-functions qr qc qe) alg)
      (set-init-qr (clone qr) alg)
      (set-init-qc (clone qc) alg)
      (set-init-qe (clone qe) alg))))
  


(defmethod print-object ((alg <hordq>) str)
  (format str "<<HORDQ alg with ~a choice stack items and ~a active exit points>>"
	  (length (choice-stack alg)) (length (active-exit-pts alg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the algorithm does in response to the various messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-env-step ((alg <hordq>) act rew to term)
  (declare (ignore to act term))
  
  (let ((tos (first (choice-stack alg)))
	(disc (discount alg)))
    
    (macrolet 
	((accumulate-reward (accumulator item)
	   (let ((x (gensym)))
	     `(let ((,x ,item))
		(incf (,accumulator ,x) (* rew (csi-discount ,x)))
		(multf (csi-discount ,x) disc)))))
    
      (assert tos nil "Choice stack was empty in inform-env-step.")
      
      ;; update Qc-acc for most recent choice on the same level
      (when (csi-visited tos)
	(accumulate-reward csi-c-acc tos))
      
      ;; for the level above the current one, update r-acc
      ;; (for levels above that, their Qr backup must have already happened)
      (accumulate-reward csi-r-acc (second (choice-stack alg)))
      
      ;; for all active exit points, update the Q_e accumulator
      (dolist (item (active-exit-pts alg))
	(accumulate-reward csi-e-acc item)))))
    

(defmethod inform-alisp-step ((alg <hordq>) omega u)
  
  (assert (not (exit-state? omega)))
  
  (unless (action-state? omega)
    (let ((tos (first (choice-stack alg)))
	  (par (second (choice-stack alg)))
	  (best-u 
	   (handler-bind
	       ((unknown-state-action #'choose-randomly))
	     (best-choice (q alg) omega))))

      (handler-case
	  (let ((qr-val (q-fn:evaluate (qr alg) omega best-u))
		(qc-val (q-fn:evaluate (qc alg) omega best-u)))
	    
	    ;; if most recent choice at this level is non-nil, do Qc backup for it
	    (when (csi-visited tos)
	      (qc-backup alg tos (+ qr-val qc-val)))
    
	    ;; if most immediate parent exists, do a qr backup if necessary
	    (when (and par (not (csi-qr-backup-flag par)))
	      (setf (csi-qr-backup-flag par) t)
	      (qr-backup alg par (+ qr-val qc-val)))
	    
	    (let* ((qe-val (q-fn:evaluate (qe alg) omega best-u))
		   (q-val (+ qe-val qr-val qc-val)))

	      ;; if most recent choice at this level is non-nil, do a Qe backup
	      (when (csi-visited tos)
		(qe-backup alg tos qe-val))
	      
	      ;; also do Qe-backups for all active exit points
	      (while (active-exit-pts alg)
		(let ((item (pop (hordq-active-exit-pts alg))))
		  (qe-backup alg item q-val)))))
	
	;; if any q-component value is undefined, don't do any more backups
	(unknown-state-action ()))

      ;; update the top choice stack entry to refer to the new choice
      (setf (csi-omega tos) omega
	    (csi-u tos) u
	    (csi-visited tos) t
	    (csi-qr-backup-flag tos) nil
	    (csi-e-acc tos) 0
	    (csi-r-acc tos) 0
	    (csi-c-acc tos) 0
	    (csi-lrate tos) (lrate:get-rate (lrate alg) (cons omega u))
	    (csi-discount tos) 1)
    
      ;; push a new empty entry onto the choice stack for the new level
      (push (make-csi) (hordq-choice-stack alg)))))



(defmethod inform-end-choice-block ((alg <hordq>) omega)
  (declare (ignore omega))

  ;; pop stack and note exited csi
  (let ((exited (pop (hordq-choice-stack alg)))
	(current (first (choice-stack alg))))
    
    (assert (and exited current))
    
    
    (cond
     
     ;; if there were any choices visited at lower levels, do qc backups
     ;; and push them onto active exit point list to await qe backup
     ((csi-visited exited) 
      (qc-backup alg exited 0)
      (push exited (hordq-active-exit-pts alg)))
     
     ;; otherwise, no qc and qe backups are needed, but a qr backup might be
     ;; needed for the previous choice at this level
     (t
      (assert (not (csi-qr-backup-flag current)))
      (qr-backup alg current 0)
      (setf (csi-qr-backup-flag current) t)))))
     
	  

(defmethod inform-part-prog-terminated ((alg <hordq>) omega)
  (declare (ignore omega))
  
  ;; there must only be one choice on the stack at this point
  (assert (= 1 (length (choice-stack alg))))
  
  (let ((tos (first (choice-stack alg))))
    
    ;; if a choice happened at top level do qc and qe backups
    (when (csi-visited tos)
      (qc-backup alg tos 0)
      (qe-backup alg tos 0))
    
    ;; for each active exit point do a qe-backup
    (while (active-exit-pts alg)
      (let ((item (pop (hordq-active-exit-pts alg))))
	(qe-backup alg item 0)))
    
    
    ;; finally remove the top item in the stack
    (setf (hordq-choice-stack alg) nil)))


(defmethod inform-start-episode ((alg <hordq>) s)
  (declare (ignore s))
  
  ;; initialize the choice stack and exit points list
  (setf (hordq-choice-stack alg) (list (make-csi)))
  (setf (hordq-active-exit-pts alg) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other learning-algorithm methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <hordq>))
  "reset HORDQ.  Reset q function components to the initial values that were supplied during object creation."
  (set-qr (clone (init-qr alg)) alg)
  (set-qc (clone (init-qc alg)) alg)
  (set-qe (clone (init-qe alg)) alg)
  (set-q (sum-q-functions (qr alg) (qc alg) (qe alg)) alg)  
  (setf (hordq-choice-stack alg) nil
	(hordq-active-exit-pts alg) nil))


(defmethod knowledge-state ((alg <hordq>) &optional (fresh t))
  (let ((q (q alg)))
    (if fresh 
	(clone q)
      q)))

(defmethod get-q-fn ((alg <hordq>) ks)
  ks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun qr-backup (alg c final-val)
  (debug-msg alg "~&Qr-backup to ~a with val ~a" c (+ (csi-r-acc c) final-val))
  (backup c (qr alg) (csi-r-acc c) final-val))

(defun qc-backup (alg c final-val)
  (debug-msg alg "~&Qc-backup to ~a with val ~a" c (+ (csi-c-acc c) final-val))
  (backup c (qc alg) (csi-c-acc c) final-val))

(defun qe-backup (alg c final-val)
  (debug-msg alg "~&Qe-backup to ~a with val ~a" c (+ final-val (csi-e-acc c)))
  (backup c (qe alg) (csi-e-acc c) final-val))

(defun backup (c q-fn rew-acc final-val)
  "backup CSI Q-FN REW-ACC FINAL-VAL.  Do a backup for the given q function and choice-stack item, assuming that REW-ACC units of discounted reward have been received, and the future reward is estimated by FINAL-VAL"
  (q-function:update q-fn (csi-omega c) (csi-u c)
		     (+ rew-acc (* (csi-discount c) final-val))
		     (csi-lrate c)))
