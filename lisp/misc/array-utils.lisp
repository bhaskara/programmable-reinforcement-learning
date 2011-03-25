;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; array-utils.lisp
;;
;; general utilities for handling arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)


(defun eql-array (a1 a2)
  "eql-array A1 A2.  return true iff arrays have the same size and corresponding elements are eql."
  (and (equal (array-dimensions a1) (array-dimensions a2))
       (loop
	   for i below (array-total-size a1)
	   do (if (not (eql (row-major-aref a1 i) (row-major-aref a2 i)))
		  (return nil))
	   finally (return t))))


(defun round-array (a d)
  "round-array A D.  return a new array in which all elements of array a are rounded to d decimal places (usually used for displaying).  Coerces all elements to floats in the process.  Inefficient implementation intended only for debugging."
  
  (let* ((dims (array-dimensions a))
	 (is-1d (= (length dims) 1)))
    (condlet ((is-1d (b-dims (length a)) (tot-size (length a)))
	      ((not is-1d) (b-dims dims) (tot-size (array-total-size a))))
	     (loop
		 with b = (make-array b-dims)
	       
		 for i below tot-size
		 do (setf (row-major-aref b i)
		      (round-decimal (row-major-aref a i) d))
		    
	 
		 finally (return b)))))


(defun map-array (f &rest arrays)
  "map-array FN &rest ARRAYS.  ARRAYS = A1...An must all be arrays with the same dimensions (and n must be >= 1).  Create a new array with the same dimension, where A[indices] = FN (A1[indices] A2[indices]... An[indices])."
  (let ((dims (array-dimensions (first arrays))))
    (assert
	(loop 
	    for a in (rest arrays)
	    always (equal dims (array-dimensions a)))
	nil "All array arguments to map-array must have same dimensions.")
    (loop
	with result = (make-array dims)
	for i below (array-total-size result)
	do (setf (row-major-aref result i)
	     (apply f (mapcar (lambda (x) (row-major-aref x i)) arrays)))
	finally (return result))))

(defun a+ (&rest arrays)
  "a+ &rest ARRAYS.  Elementwise addition."
  (apply #'map-array #'+ arrays))


(defun a- (&rest arrays)
  "a- &rest ARRAYS.  Elementwise subtraction."
  (apply #'map-array #'- arrays))

(defun a* (a1 a2)
  "a* A1 A2.  Various cases
1) A1 is a number and A2 is any array (or vice versa), then multiply each element of A2 by A1 
2) Both are vectors.  Do an inner-product
3) A1 is a vector, A2 is a matrix.  Do a row-vector x matrix multiplication
4) A1 is a matrix, A2 is a vector.  Do a matrix x column-vector multiplication
5) Both are matrices.  Do a matrix multiplication.

Otherwise error.  In cases 2-5, error if dimensions don't match up."
  
  (if (numberp a1)
      (map-array (lambda (x) (* x a1)) a2)
    (if (numberp a2)
	(map-array (lambda (x) (* x a2)) a1)
      (let* ((d1 (array-dimensions a1))
	     (d2 (array-dimensions a2))

	     ;; not the right way to do it (and inefficient since a string is being created each time).
	     ;; should signal a condition instead.
	     (dim-err (format nil "Array dimensions ~a and ~a do not match in a*" d1 d2)))
	(cond ((and (= (length d1) 1) (= (length d2) 1))
	       (assert (= (first d1) (first d2)) nil dim-err)
	       (loop for x across a1 for y across a2 sum (* x y)))
	      ((and (= (length d1) 1) (= (length d2) 2))
	       (assert (= (first d1) (first d2)) nil dim-err)
	       (map 'vector
		 (lambda (i) 
		   (loop 
		       for x across a1
		       for j from 0
		       for y = (aref a2 j i)
		       sum (* x y)))
		 (below (second d2))))
	      ((and (= (length d1) 2) (= (length d2) 1))
	       (assert (= (second d1) (first d2)) nil dim-err)
	       (map 'vector
		 (lambda (i)
		   (loop
		       for y across a2
		       for j from 0
		       for x = (aref a1 i j)
		       sum (* x y)))
		 (below (first d1))))
	      ((and (= (length d1) 2) (= (length d2) 2))
	       (assert (= (second d1) (first d2)) nil dim-err)
	       (loop
		   with prod = (make-array (list (first d1) (second d2)) :element-type 'float)
		   for i below (first d1)
		   do (loop
			  for j below (second d2)
			  do (setf (aref prod i j)
			       (loop for k below (first d2) summing (* (aref a1 i k) (aref a2 k j)))))
		   finally (return prod)))
			    
	    
	      (t (assert nil nil "Unhandled case in a*")))))))
					     


(defun 1s (&rest dims)
  "1s &rest DIMS.  Make an array of element-type float with dimensions DIMS containing all 1s."
  (make-array dims :element-type 'float :initial-element 1.0))

(defun 0s (&rest dims)
  "0s &rest DIMS.  Make an array of element-type float with dimensions DIMS containing all 0s."
  (make-array dims :element-type 'float :initial-element 0.0))


(defun array-size-multipliers (a)
  "array-size-multipliers A.  Return the multipliers needed to step through the various dimensions of the array.  For example, an array with dimensions 3,5,4,2 would have multipliers 40,8,2,1"
  (loop
      with step = 1
      with mults = nil
      for mult in (cons 1 (reverse (rest (array-dimensions a))))
      do (push (setf step (* step mult)) mults)
      finally (return mults)))
		  





(defun subarray (a inds)
  "subarray A INDS.  Returns an array consisting of all entries of A whose index begins with INDS (which is a list).  E.g., if A has dimensions 2,3,4,3 then subarray(a,(1 2)) will return the 4x3 array a[1,2,0,0],...,a[1,2,3,2].  Note that the new array shares structure with the original one, i.e. if the entries are modified, then the original array will be changed as well."
  (let* ((offset 0)
	 (newdims
	  (loop
	      with indices = inds
	      for dim in (array-dimensions a)
	      for mult in (array-size-multipliers a)
	      for ind = (first indices)

	      if ind
	      do (setf indices (rest indices)
		       offset (+ offset (* mult ind)))
		 
	      else
	      collect dim)))


		 
    (make-array newdims :displaced-to a :displaced-index-offset offset)))
	
(defun set-subarray (a inds new-vals)
  (loop
      for ind from (loop 
		       for i in inds 
		       for mult in (array-size-multipliers a)
		       sum (* i mult))
      for x across new-vals
      do (setf (row-major-aref a ind) x)
      finally (return new-vals)))
  

(defsetf subarray set-subarray)



(defun reshape-array (a dims &key (clone-p nil))
  "reshape-array A DIMS &key (CLONE-P nil).  Reshape array to new dimensions DIMS (which must have the right total number of elements).  If CLONE-P is true, then items are cloned."
  (let ((old-dims (array-dimensions a)))
    (assert (equal (apply #'* dims) (apply #'* old-dims))
	() "Array of size ~a cannot be converted to array of size ~a!" old-dims dims))
  
  (loop
      with new-a = (make-array dims)
      for i below (array-total-size a)
      do (setf (row-major-aref new-a i)
	   (let ((x (row-major-aref a i)))
	     (if clone-p
		 (clone x)
	       x)))
      finally (return new-a)))


(defun sparsify (v &key test (name-fn #'identity))
  "sparsify V &optional TEST NAME-FN.  Return a vector w each of whose elements is a pair (ind . val), which  means that v(ind) = val, for all the nonzero elements of v.  w is in increasing order of ind.  TEST is used to select which elements are included.  By default, all elements that are nonzero are included.  NAME-FN is applied to ind before printing it.  By default, its the identity function."
  (loop
      with w = (make-array 0 :adjustable t :fill-pointer 0)
      with include? = (or test (lambda (x) (/= x 0)))
      for x across v
      for i from 0
      when (funcall include? x)
      do (vector-push-extend (cons (funcall name-fn i) x) w)
      finally (return w)))


  
  



(in-package cl-user)

