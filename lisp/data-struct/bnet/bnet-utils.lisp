(in-package bnet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; topological sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun topological-sort (parents)
  "topological-sort PARS.  PARS is a vector of parent-index lists.  Return topological ordering.  E.g. if PARS is #((1 2) () (1)) then returns '(1 2 0).  The sort is also stable, i.e., if two variables are incomparable, then the one with the smaller index comes first.  Assert if there's a cycle."
  (let ((desc (descendant-matrix parents)))
    (stable-sort (below (length parents))
		 (lambda (i j) (aref desc j i)))))

(defun descendant-matrix (pars)
  (let* ((n (length pars))
	 (a (make-array (list n n) :element-type 'boolean :initial-element nil))
	 (done? nil))

    (flet ((fill-in-descendant-matrix ()
	     (until done?
	       (setf done? t)
	       (loop
		   for i below n
		   for p across pars
		   do (dotimes (j n)
			(when (or (= i j) (aref a j i))
			  (dolist (k p)
			    (unless (aref a j k)
			      (setf (aref a j k) t
				    done? nil)))))))))
      
      (fill-in-descendant-matrix)
      
      ;; then add in incomparable elements
      (dotimes (i n)
	(do ((j (1+ i) (incf j)))
	    ((= j n))
	  (unless (or (aref a i j) (aref a j i))
	    (if (loop for k below n thereis (and (aref a i k) (aref a k j)))
		(setf (aref a i j) t)
	      (setf (aref a j i) t)))))
      
      ;; make sure nothing is less than itself
      (assert (loop
		  for i below n
		  never (aref a i i))
	  () "Cyclic dependency in parent list ~a" pars)
        
      a)))




		   
	    
