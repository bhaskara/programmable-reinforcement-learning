(defpackage circular-vector
  (:use cl
	utils)
  (:nicknames circ-vec)
  (:export
   circular-vector
   make-circular-vector
   circular-push
   get-flat-vector)
  (:documentation "
Package circular-vector (circ-vec).
Implements the circular-vector data type.

Type
----
circular-vector

Constructor
-----------
make-circular-vector

Operations
----------
circular-push
get-flat-vector"))

(in-package circular-vector)


(defstruct (circular-vector (:conc-name nil) (:constructor create-circ-vec))
  "A circular-vector is a data type that is useful for storing logs and histories.  It is like a standard vector, except after we reach a predefined number of items, we wrap back to the beginning (overwriting what was there before).  

Operations
----------
make-circular-vector
circular-push
get-flat-vector


Example
-------
With a circular vector of length 3, here is a sequence of push and get-flat-vector operations.

#()
Push 3 -> #(3)
Push 2 -> #(3 2)
Push 4 -> #(3 2 4)
Push 1 -> #(2 4 1)
Push 5 -> #(4 1 5)"
  vec
  start
  num)

(defun make-circular-vector (length)
  "make-circular-vector LENGTH.  Creates a circular-vector of length LENGTH (a fixnum)."
  (create-circ-vec :vec (make-array length) :start 0 :num 0))

(defun get-flat-vector (v)
  "get-flat-vector CIRCULAR-VECTOR.  Return a new vector containing the elements of CIRCULAR-VECTOR in the correct order."
  (let ((n (num v))
	(j (start v))
	(vec (vec v)))
    (let ((a (make-array n))
	  (l (length vec)))
      (dotimes (i n a)
	(setf (aref a i) (aref vec j))
	(when (>= (incf j) l)
	  (setf j 0))))))

(defun circular-push (x v)
  "circular-push ITEM CIRCULAR-VECTOR.  Add the ITEM to the end of the CIRCULAR-VECTOR.  If have reached the maximum number of items, this might loop back to the beginning, overwriting existing items."
  (let ((vec (vec v))
	(n (num v))
	(j (start v)))
    (if (= n (length vec))
	(setf (aref vec j) x
	      (start v) (if (>= j (- n 1))
			    0
			  (1+ j)))
      (setf (aref vec n) x
	    (num v) (1+ n)))))
      


  
  
   