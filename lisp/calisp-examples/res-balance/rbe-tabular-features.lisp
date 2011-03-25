(defpackage rbe-tabular-features
  (:documentation
   "Tabular features for calisp resource balance program")
  (:use 
   cl
   rbe-prog
   calisp-features
   rbe
   utils)
  (:export
   make-tabular-featurizer))
  

(in-package rbe-tabular-features)
   


(defun make-tabular-featurizer (wm)
  (lambda (omega u)
    (let* ((choosing-thread-ids
	    (js-choosing-thread-ids omega))
	   (choosing-thread-labels
	    (mapcar (lambda (id)
		      (js-thread-label omega id))
		    choosing-thread-ids))
	   (env-state (js-env-state omega))
	   (gold (gold env-state))
	   (wood (wood env-state))
	   (state-type 
	    (if (eq (first choosing-thread-labels) 'nav-choice)
		'nav-choice
	      (when (and (eq (first choosing-thread-labels) 'task-choice)
			 (zerop gold) (zerop wood))
		'init-state)))
	   (l (list
	       (loop 
		   for k being each hash-key in (js-thread-states omega) using (hash-value v)
		   collect (list k (ts-label v) (ts-stack v)))
	       (list 'resources (gold env-state) (wood env-state)))))
      

      (case state-type
	(nav-choice (cons (result-positions wm (pos env-state) choosing-thread-ids u) l))
	(init-state (list u l))
	(otherwise (list* (pos env-state) u l))))))

(defun result-positions (wm positions t-ids u)
  (let ((a (make-array (length positions) :initial-element 'uninitialized)))
    (mapc (lambda (id act)
	    (setf (aref a id)
	      (gw:result-legal wm (aref positions id) act)))
	  t-ids u)
    (dotimes (i (length a) a)
      (when (eq (aref a i) 'uninitialized)
	(setf (aref a i) (aref positions i))))))



