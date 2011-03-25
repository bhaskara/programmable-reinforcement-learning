;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/bucketed-counts.lisp
;; TODO - should get rid of this
;;
;; Defines the <bucketed-counts> class, for the following situation.  Suppose there is a set of 
;; objects, which are grouped into buckets.  Each bucket has a count which starts at 1.  There is
;; also a function bucket-val which maps a bucket and count to a single-float.  
;; Calling the method (get-current-val X) increments the count for X's bucket and returns 
;; bucket-val applied to the old count.  Subclasses of this are used for learning rates and 
;; exploration policies.
;;
;; Class initialization arguments
;; - :bucket-fn
;;
;; Externally called methods
;; - get-current-val
;; - reset-table
;; - reset-counts
;;
;; Methods to be overridden by subclasses
;; - bucket-val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage bucketed-counts
  (:use common-lisp)
  (:export <bucketed-counts>
	   get-current-val
	   bucket-val
	   bucket-function
	   reset-table
	   reset-counts))
  

(in-package bucketed-counts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <bucketed-counts> ()
  ((count-table :type hash-table
		:initform (make-hash-table :test #'equal)
		:reader count-table
		:documentation "stores table of counts"
		:writer set-count-table)
   (bucket-function :type function
		    :initarg :bucket-fn
		    :initform (lambda (x) x)
		    :documentation "function that maps objects to the key of their bucket"
		    :reader bucket-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods to be called by outside world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reset-table (bc)
  (:documentation "reset-table BC.  Reset the count table of BC to be empty.")
  (:method ((bc <bucketed-counts>))
	    (set-count-table (make-hash-table :test #'equal) bc)))

(defgeneric reset-counts (bc)
  (:documentation "reset-counts BC.  Reset all the counts currently in BC's table to 1.  Preferable to reset-table if the same bucket set is going to be used again.")
  (:method ((bc <bucketed-counts>))
	   (loop 
	       for b being each hash-key in (count-table bc) 
	       do (setf (gethash b (count-table bc)) 1))))

(defgeneric get-current-val (bc x)
  (:documentation "get-current-val BC X.  Get the current value for the bucket of X and increment its count.  If X's bucket is not currently in the count table, it is first added with a count of 1.")
  (:method ((bc <bucketed-counts>) x)
	   (let ((b (funcall (bucket-fn bc) x)))
	     (bucket-val bc (float (get-and-inc-count bc b)) b))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bucket-val (bc count bucket)
  (:documentation "bucket-val BC COUNT BUCKET.  Return value of bucket BUCKET having count COUNT.  Default is just to return COUNT.")
  (:method ((bc <bucketed-counts>) count bucket)
	   (declare (ignore bucket))
	   count))

(defun get-and-inc-count (bc bucket)
  "get-and-inc-count BC BUCKET.  Increment the count for BUCKET and return the old value.  If it is not in the table, return 1 and add it with a count of 2."
  (let* ((count-table (count-table bc))
	 (count (or (gethash bucket count-table) 1)))
    (setf (gethash bucket count-table) (1+ count))
    count))


(defun print-bc (bc)
  "debug method that prints the counts and associated values"
  (loop
      with h = (count-table bc)
      initially (format t "~&Printing bucket-count hash table")
      for b being each hash-key in h using (hash-value v)
      for r = (bucket-val bc v b)
      do (format t "~&Bucket : ~a Count : ~a  Value : ~a" b v r)))
	 


(in-package cl-user)

	    
	   
   


