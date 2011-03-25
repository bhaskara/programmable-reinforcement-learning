(in-package lrate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <polynomial-learning-rate> (<learning-rate>)
  ((a :type single-float
      :initarg :a
      :reader a
      :initform 0)
   (b :type single-float
      :initarg :b
      :reader b
      :initform 1)
   (n :type single-float
      :initarg :n
      :reader n
      :initform 1))
  (:documentation "Class <polynomial-learning-rate>.  

Initargs
:a, :b, :n - parameters of the learning rate function 1/(a+b*count)^n.  
Default values are a=0, b=n=1.
:bucket-fn (inherited from <bucketed-counts>) - maps items to 
their bucket ID, so items with #'equal bucket IDs share a count.  Default 
value is #'identity."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-constant-lrate (c)
  "make-constant-lrate C."
  (make-instance '<polynomial-learning-rate> :a (/ 1 c) :b 0 :n 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bucket-val ((lr <polynomial-learning-rate>) count bucket)
  "Inverse polynomial decay function.  1/(a+b*t^n)"
  (declare (ignore bucket))
  (/ 1 (+ (a lr) (* (b lr) (expt count (n lr))))))



(in-package cl-user)

	    
	   
   


