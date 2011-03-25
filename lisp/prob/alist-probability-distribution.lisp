;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prob/alist-probability-distribution
;; Code for treating alists as probability distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package prob)

(defmethod sample ((l list))
  (let ((x (random 1.0))
	(cumsum 0))
    (dolist (entry l)
      (incf cumsum (cdr entry))
      (when (> cumsum x)
	(return (car entry))))))

(defmethod prob ((l list) x)
  (let ((entry (assoc x l :test #'utils:same)))
    (aif entry
	(cdr it)
	(progn 
	  ;; TODO - should make this a cerror
	  (signal 'element-not-in-domain)
	  0.0))))

(defmethod expectation ((l list) rv)
  (reduce #'+ l
	  :key #'(lambda (entry)
		   (let ((prob (cdr entry)))
		     (if (> prob 0)
			 (* prob (evaluate-rv rv (car entry)))
		       0)))))



(defmethod sample-space ((l list))
  (mapcar #'car l))

(defmethod sample-iterator ((l list))
  (let ((x l))
    (lambda ()
      (if x
	  (let ((item (car x)))
	    (setf x (cdr x))
	    (values (car item) (cdr item) nil))
	(values nil nil t)))))

(defun standardize-alist-prob-dist (l)
  "standardize-alist-prob-dist L.  L is a list of elements of form (ELT . PROB).  Collects together any ELT that are the same, and normalizes PROBs to sum to 1 (assuming they're all > 0)."
  (loop
      with new = nil
      with prob = 0
      for x in l
      do (let ((existing (find x new :test (lambda (x y) (same (car x) (car y))))))
	   (if existing
	       (incf (cdr existing) (cdr x))
	     (push x new)))
	 (assert (>= prob 0))
	 (incf prob (cdr x))

      finally 
	(assert (> prob 0))
	(map nil (lambda (x) (setf (cdr x) (/ (cdr x) prob))) new)
	(return new)))


(defmethod update-dist ((d1 list) p eta)
  (dolist (x d1)
    (setf (cdr x) (+ (* (- 1 eta) (cdr x))
		     (* eta (prob p (car x))))))
  (do-sample-points (x prob p)
    (unless (member x d1 :test #'same :key #'car)
      (push (cons x (* eta prob)) d1)))
  d1)
  

(defmethod clone-dist ((l list))
  (mapcar (lambda (x) (cons (car x) (cdr x))) l))
	    