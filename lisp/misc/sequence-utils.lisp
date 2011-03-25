(in-package utils)

(defconstant *default-tail-length* 10)

(defun stail (s &optional (tail-length *default-tail-length*))
  "stail SEQ &optional (LENGTH 10).  Return last LENGTH elements of SEQ."
  (if (> (length s) tail-length)
      (subseq s (- (length s) tail-length))
    s))

(defun slast (s)
  "slast SEQ.  Last element of SEQ."
  (assert (> (length s) 0))
  (elt s (1- (length s))))


(defmacro def-seq-accessor (name pos)
  (let ((s (gensym)))
    `(defun ,name (,s) (elt ,s ,pos))))

(def-seq-accessor sfirst 0)
(def-seq-accessor ssecond 1)
(def-seq-accessor sthird 2)
(def-seq-accessor sfourth 3)


(defgeneric is-sorted (l &optional pred)
  (:documentation "is-sorted SEQUENCE &OPTIONAL (PRED #'<=).  Do every pair of adjacent elements in SEQUENCE satisfy PRED?  (We're assuming PRED is transitive)")
  (:method ((l list) &optional (pred #'<=))
	   (loop
	       for rem on l
	       always (or (not (cdr rem)) (funcall pred (car rem) (cadr rem)))))
  (:method ((l vector) &optional (pred #'<=))
	   (loop
	       for i below (1- (length l))
	       always (funcall pred (aref l i) (aref l (1+ i))))))

