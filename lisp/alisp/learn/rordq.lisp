(in-package alisp-hordq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <rordq> (<hordq>)
  ()
  (:documentation "Like HORDQ, except without Qe (so it's implicitly set to 0 in the tabular case).

Initargs
:lrate - learning rate
:discount - discount factor
:hist-inc - how often to save hist
:qr -  Q-function object for qr component.
:qc -  Q-function object for qc component.
:debug-str - nil by default.  If non-nil, the state of the alg is logged to this stream

"))

(defmethod print-object ((alg <rordq>) str)
  (format str "<RORDQ alg with ~a choice stack items"
	  (length (choice-stack alg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the algorithm does in response to the various messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-env-step ((alg <rordq>) act rew to term)
  (declare (ignore to act term))
  
  (let ((stack (choice-stack alg))
	(disc (discount alg)))
    
    ;; do a Qr backup for level above current one
    (let ((item (second stack)))
      (qr-backup alg item rew)
      (setf (csi-qr-backup-flag item) t))
    
    ;; update discount factors above current level
    (dolist (item (cdr stack))
      (multf (csi-discount item) disc))))
    

(defmethod inform-alisp-step ((alg <rordq>) omega u)
  (if (exit-state? omega)
      (rordq-handle-exit-state alg omega)
    (rordq-handle-non-exit-state alg omega u)))




(defun rordq-handle-non-exit-state (alg omega u)
  
  (let ((tos (first (choice-stack alg)))
	(par (second (choice-stack alg)))
	(best 
	 (handler-bind ((unknown-state-action #'choose-randomly))
	   (best-choice (q alg) omega))))
    
    (handler-case
	(let ((qr-val (q-fn:evaluate (qr alg) omega best))
	      (qc-val (q-fn:evaluate (qc alg) omega best)))
    
	  ;; if most recent choice at this level is non-nil, do Qc backup
	  (when (csi-visited tos)
	    (qc-backup alg tos (+ qr-val qc-val)))
    
	  ;; if most immediate parent exists, and its qr-comp hasn't been backed up yet, do so
	  (when (and par (not (csi-qr-backup-flag par)))
	    (qr-backup alg par (+ qr-val qc-val))))
      (unknown-state-action ()))


    ;; if most recent choice at this level is non-nil, do Qe backup of 0
    (when (csi-visited tos)
      (qe-backup alg tos 0))

    ;; in any case, prevent further Qr backups to parent
    (when par
      (setf (csi-qr-backup-flag par) t))
    
    (setf (csi-omega tos) omega
	  (csi-u tos) u
	  (csi-visited tos) t
	  (csi-qr-backup-flag tos) nil
	  (csi-e-acc tos) 0
	  (csi-lrate tos) (lrate:get-rate (lrate alg) (cons omega u))
	  (csi-discount tos) 1)
    
    ;; push a new empty entry onto the choice stack for the new level
    (push (make-csi) (hordq-choice-stack alg))))



(defun rordq-handle-exit-state (alg omega)
  
  (let ((exited (pop (hordq-choice-stack alg)))
	(current (first (choice-stack alg))))
    
    (handler-case
	
	(let ((qc-val (q-fn:evaluate (qc alg) omega 'no-choice))
	      (qe-val 0)) ;; dummy value for qe
    
	  (assert (and exited current))
    
	  ;; do qc and qe backups for the previous choice point
	  ;; (which must exist, since it's where we entered this choice block)
	  (qe-backup alg current qe-val)
	  (qc-backup alg current qc-val) ; actually qr + qc, but qr here is 0
	  
	  (when (csi-visited exited)
	    (qe-backup alg exited (+ qc-val qe-val))))
      
      (unknown-state-action ()))
    
    (cond
     ;; if there were any choices visited at lower levels, do a qc
     ;; and qe backup for the last one
     ((csi-visited exited) 
      (qc-backup alg exited 0))

     ;; otherwise, no qc and qe backups are needed, but a qr backup 
     ;; may be needed for the previous choice at this level
     ((not (csi-qr-backup-flag current))
      (qr-backup alg current 0)
      (setf (csi-qr-backup-flag current) t)))
    

    ;; update the choice stack entry at the level of omega
    (setf (csi-omega current) omega
	  (csi-u current) 'no-choice
	  (csi-lrate current) (lrate:get-rate (lrate alg) (cons omega 'no-choice))
	  (csi-discount current) 1)
    
    ;; Make sure Qr of exit state gets set to 0
    (qr-backup alg current 0)))

    
     
	  

(defmethod inform-part-prog-terminated ((alg <rordq>) omega)
  (declare (ignore omega))
  
  ;; there must only be one choice on the stack at this point
  (assert (= 1 (length (choice-stack alg))))

  (let ((tos (first (choice-stack alg))))
    
    ;; if a choice happened at top level do qc and qe backups
    (when (csi-visited tos)
      (qc-backup alg tos 0)
      (qe-backup alg tos 0))
    
    ;; finally remove the top item in the stack
    (setf (hordq-choice-stack alg) nil)))


(defmethod inform-start-episode ((alg <rordq>) s)
  (declare (ignore s))
  ;; initialize the choice stack and exit points list
  (setf (hordq-choice-stack alg) (list (make-csi))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other learning-algorithm methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((alg <rordq>))
  "reset RORDQ.  Reset q function components to the initial values that were supplied during object creation."
  (set-qr (clone (init-qr alg)) alg)
  (set-qc (clone (init-qc alg)) alg)
  (set-qe (clone (init-qe alg)) alg)
  (set-q (q-fn:sum-q-functions (qr alg) (qc alg) (qe alg)) alg)  
  (setf (hordq-choice-stack alg) nil))


;; knowledge-state and get-q-fn as in <hordq>


