;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/learn/holyq.lisp
;; Hierarchically Optimal Yet Local Q learning Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp-hordq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; csi (choice-stack item) defined in hordq.lisp


(defclass <holyq> (<alisp-learning-algorithm> <q-learning-algorithm>)
  ((choice-stack 
    :type list
    :reader choice-stack
    :initform nil
    :accessor holyq-choice-stack)
   (pe :reader pe :writer set-pe :initarg :pe) ;; replaces qe
   (init-pe :reader init-pe :writer set-init-pe)
   (discount
    :reader discount :initform 1.0
    :initarg :discount)
   (qr :reader qr :writer set-qr :initarg :qr)
   (qc :reader qc :writer set-qc :initarg :qc)
   (init-qr :reader init-qr :writer set-init-qr)
   (init-qc :reader init-qc :writer set-init-qc)
   (q :reader q :writer set-q)
   (learning-rate :reader lrate :initform .02
		  :initarg :lrate
		  :type lrate:<learning-rate>))
  
  (:documentation "Class <holyq>.  Does hierarchical q-learning, using the 2.5-part decomposition Q = Qr + Qc + PeV

Initargs
:lrate - learning rate
:discount - discount factor
:hist-inc - how often to save hist
:qr -  Q-function object for qr component.
:qc -  Q-function object for qc component.
:pe -  Exit probability distribution
:debug-str - nil by default.  If non-nil, the state of the alg is logged to this stream
:features

"))

(defmethod initialize-instance :after ((alg <holyq>) &rest args 
				       &key (features nil))
  (labels ((make-q-fn-approx ()
	     (make-instance 'fn-approx:<tabular-fn-approx>))
	   
	   (make-q-fn (feats)
	     (if feats
		 (make-instance '<alisp-approx-q-function> :featurizer feats
				:fn-approx (make-q-fn-approx))
	       (make-instance '<alisp-approx-q-function> :fn-approx (make-q-fn-approx))))
		 
	   (make-exit-dist (feats)
	     (let ((featurizer (or feats #'cons)))
	       (make-instance 'aed:<hash-exit-distribution> :featurizer featurizer))))
    
    (let ((qr (make-q-fn (first features)))
	  (qc (make-q-fn (second features)))
	  (pe (make-exit-dist (third features))))
      (unless (slot-boundp alg 'qr) (set-qr qr alg))
      (unless (slot-boundp alg 'qc) (set-qc qc alg))
      (unless (slot-boundp alg 'pe) (set-pe pe alg))
      (set-q (make-holy-q-fn (qr alg) (qc alg) (pe alg)) alg)
      (set-init-qr (clone (qr alg)) alg)
      (set-init-qc (clone (qc alg)) alg)
      (set-init-pe (clone (pe alg)) alg))))


  

(defmethod print-object ((alg <holyq>) str)
  (format str "<HOLYQ alg with ~a choice stack items>" (length (choice-stack alg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the algorithm does in response to the various messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-env-step ((alg <holyq>) act rew to term)
  (declare (ignore to act term))
  
  (let ((stack (choice-stack alg))
	(disc (discount alg)))
    
    ;; do a Qr backup for level above current one
    (let ((item (second stack)))
      (qr-backup alg item rew)
      (setf (csi-qr-backup-flag item) t))
    
    ;; update discount factors above current level
    (dolist (item (rest stack))
      (multf (csi-discount item) disc))))
    
	 

(defmethod inform-alisp-step ((alg <holyq>) omega u)
  
  (if (exit-state? omega)
      (holyq-handle-exit-state alg omega)
    (holyq-handle-non-exit-state alg omega u)))

(defun holyq-handle-non-exit-state (alg omega u)
  (let ((tos (first (choice-stack alg)))
	(par (second (choice-stack alg)))
	(best-u 
	 (handler-bind
	     ((unknown-state-action #'choose-randomly))
	   (best-choice (q alg) omega))))
    
    
    ;; If Qr and Qc are defined at this state...
    (handler-case
	(let ((qr-val (q-fn:evaluate (qr alg) omega best-u))
	      (qc-val (q-fn:evaluate (qc alg) omega best-u)))
	  
	  ;; ...if most recent choice at this level is non-nil, do a Qc backup for it
	  (when (csi-visited tos)
	    (qc-backup alg tos (+ qr-val qc-val)))
	  
	  ;; ...if most immediate parent exists, and its qr-comp hasn't been backed up yet, do so
	  (when (and par (not (csi-qr-backup-flag par)))
	    (qr-backup alg par (+ qr-val qc-val))))
      
      ;; If they're not defined, don't do the backups
      (unknown-state-action ()))
    
    
    ;; Backup Pe
    (let ((exit-dist (aed:exit-dist (pe alg) omega best-u)))
      (when (csi-visited tos)
	(pe-backup alg tos exit-dist)))

    
    
    ;; in any case, we don't want further Qr backups to happen for the parent state
    (awhen par (setf (csi-qr-backup-flag it) t))
    
    
    ;; update the choice stack entry at the level of omega
    (setf (csi-omega tos) omega
	  (csi-u tos) u
	  (csi-visited tos) t
	  (csi-qr-backup-flag tos) nil
	  (csi-lrate tos) (lrate:get-rate (lrate alg) (cons omega u))
	  (csi-discount tos) 1)
    
    ;; push a new empty entry onto the choice stack for the new level
    (push (make-csi) (holyq-choice-stack alg))))


(defun holyq-handle-exit-state (alg omega)

  (let ((exited (pop (holyq-choice-stack alg)))
	(current (first (choice-stack alg))))
    
    ;; If Qc defined, do backup
    (handler-case
	(let ((qc-val (q-fn:evaluate (qc alg) omega 'no-choice)))
	  (qc-backup alg current qc-val))
      (unknown-state-action ()))
    
    ;; Backup Pe
    (let ((exit-dist (aed:exit-dist (pe alg) omega 'no-choice)))
      (pe-backup alg current exit-dist))
      
    (cond
     ;; if there were any choices visited at lower levels, do a
     ;; qc and pe backup for the last one
     ((csi-visited exited) 
      (qc-backup alg exited 0)
      (pe-backup alg exited (make-deterministic-dist omega)))
     
     ;; otherwise, no qc and qe backups are needed, but a qr backup may
     ;; be needed for the previous choice at this level
     ((not (csi-qr-backup-flag current))
      (qr-backup alg current 0)
      (setf (csi-qr-backup-flag current) t)))

    ;; update choice stack entry at level of omega
    (setf (csi-omega current) omega
	  (csi-u current) 'no-choice
	  (csi-lrate current) (lrate:get-rate (lrate alg) (cons omega 'no-choice))
	  (csi-discount current) 1)
    
    ;; Make sure QR value for this exit state gets set to 0 (kind of hacky)
    (qr-backup alg current 0)))
    

(defmethod inform-part-prog-terminated ((alg <holyq>) omega)
  (declare (ignore omega))
  
  ;; there must only be one choice on the stack at this point
  (assert (= 1 (length (choice-stack alg))))

  (let ((tos (first (choice-stack alg))))
    
    ;; if a choice happened at top level do qc backup
    (when (csi-visited tos)
      (qc-backup alg tos 0))

    ;; remove the top item in the stack
    (setf (holyq-choice-stack alg) nil)))


(defmethod inform-start-episode ((alg <holyq>) s)
  (declare (ignore s))
  
  ;; initialize the choice stack and exit points list
  (setf (holyq-choice-stack alg) (list (make-csi))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other learning-algorithm methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <holyq>))
  "reset HOLYQ.  Reset q function components to the initial values that were supplied during object creation."
  (set-qr (clone (init-qr alg)) alg)
  (set-qc (clone (init-qc alg)) alg)
  (set-pe (clone (init-pe alg)) alg)
  (set-q (make-holy-q-fn (qr alg) (qc alg) (pe alg)) alg)
  (setf (holyq-choice-stack alg) nil))

(defmethod knowledge-state ((alg <holyq>) &optional (fresh t))
  (let ((q (q alg)))
    (if fresh 
	(clone q)
      q)))

(defmethod get-q-fn ((alg <holyq>) ks)
  ks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; qr-backup and qc-backup defined in <hordq>

(defun pe-backup (alg c dist)
  "Make exit dist of c more like dist"
  (update-exit-dist (pe alg) (csi-omega c) (csi-u c) dist (csi-lrate c)))
    
    

(defun trivial-projection (omega omega2)
  (declare (ignore omega))
  omega2)