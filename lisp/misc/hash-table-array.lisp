
(defpackage hash-table-array
  (:documentation "Defines a data type for arrays of hash tables.

Types
-----
hash-table-array

Functions
---------
make-hta
get-val
set-val")

  (:nicknames hta)
  (:use
   utils
   cl)
  (:export
   hash-table-array
   make-hta
   get-val
   set-val))

(in-package hta)

(defstruct (hash-table-array (:conc-name hta-))
  key-fn
  a)

(defun make-hta (key-fn max-key &key (test #'equalp))
  "make-hta KEY-FN MAX-KEY &key (test #'equalp)
Make an array of hash tables using KEY-FN, which maps objects to nonnegative integers.  The individual hash tables use TEST as the test."
  
  (make-hash-table-array :key-fn key-fn 
			 :a (loop with a = (make-array (1+ max-key))
				for i below (1+ max-key)
				do (setf (aref a i) (make-hash-table :test test))
				finally (return a))))

(defun get-val (x hta)
  "get-val X HTA.  Like gethash"
  (gethash (canonicalize x) (aref (hta-a hta) (funcall (hta-key-fn hta) x))))

(defun set-val (x hta new-val)
  "set-val KEY NEW-VAL HTA.  Like (setf (gethash KEY H) NEW-VAL)."
  (setf (gethash (canonicalize x) (aref (hta-a hta) (funcall (hta-key-fn hta) x)))
    new-val))

  
  