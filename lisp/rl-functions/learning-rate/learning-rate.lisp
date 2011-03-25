(defpackage learning-rate
  (:documentation "Package for learning-rates.  

Types
[learning-rate] - either <learning-rate> or a number
<learning-rate> - can be extended to create new learning-rates

Operations
- get-rate
- reset-table
- reset-counts
")
  (:nicknames lrate)
  (:use bucketed-counts
	common-lisp)
  (:export get-rate
	   reset-table
	   reset-counts
	   [learning-rate]
	   <learning-rate>))

(in-package learning-rate)


(deftype [learning-rate] ()
  "Type for learning rates.  Either
1. A number, which indicates a constant learning rate
2. An object of type <learning-rate>"
  `(or number <learning-rate>))


(defclass <learning-rate> (<bucketed-counts>)
  ()
  (:documentation "Class <learning-rate>.  Initargs
:bucket-fn - function from items to bucket ID."))


(defgeneric get-rate (lrate x)
  (:documentation "get-rate LRATE X.  Get current learning rate at X (and perhaps update counts).")
  (:method ((lrate number) x) (declare (ignore x)) lrate)
  (:method ((lrate <learning-rate>) x) (get-current-val lrate x)))


;; reset is a noop for constant learning rates
(defmethod reset-table ((lrate number)))
(defmethod reset-counts ((lrate number)))




(in-package cl-user)
	   