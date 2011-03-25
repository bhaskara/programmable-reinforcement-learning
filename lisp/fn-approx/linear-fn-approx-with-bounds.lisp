(in-package fn-approx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition of <linear-fn-approx-with-bounds> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <linear-fn-approx-with-bounds> (<linear-fn-approx>)
  ((mins :type vector :reader mins :initarg :mins :writer set-mins)
   (maxs :type vector :reader maxs :initarg :maxs :writer set-maxs))
  (:documentation "Class for linear function approximation where some bounds on parameters are known.  The online updates then ensure that these bounds hold at each stage.

Initargs
:mins - a vector.  The Ith entry is either the minimum possible of value of the Ith parameter, or NIL if no minimum is known.
:maxs - a vector.  The Ith entry is either the maximum possible of value of the Ith parameter, or NIL if no minimum is known.


Initialization arguments from <linear-fn-approx>:
:dim - dimension of parameter vector.  
:params - initial parameters.  optional, if provided must be a vector of dimension dim, defaults to #(0 0 .... 0)

"))

(defmethod initialize-instance :after ((f <linear-fn-approx-with-bounds>) &rest args &key mins maxs)
  (if (slot-boundp f 'mins)
      (dotimes (i (length mins))
	(orf (aref mins i) '-infty))
    (set-mins (make-array (dim f) :initial-element '-infty) f))
  
  (if (slot-boundp f 'maxs)
      (dotimes (i (length maxs))
	(orf (aref maxs i) 'infty))
    (set-maxs (make-array (dim f) :initial-element 'infty) f))
  
  (enforce-bounds f))



(defun enforce-bounds (f)
  (with-slots (mins maxs params) f
    (dotimes (i (length params))
      (setf (aref params i)
	(mymin (mymax (aref params i) (aref mins i)) (aref maxs i)))))
  (values))



(defmethod reset ((fa <linear-fn-approx-with-bounds>) &optional theta)
  (if theta
      (progn
	(assert (equal (length theta) (dim fa)) nil "Dimension of ~a is not ~a" theta (dim fa))
	(set-params theta fa))
    (set-params (make-array (dim fa) :initial-element 0.0) fa))
  (enforce-bounds fa)
  t)


(defmethod update ((fa <linear-fn-approx-with-bounds>) x y eta)
  "take a step in the direction of the negative gradient of the squared error given this example"
  (loop
      with theta = (params fa)
      with error = (- y (evaluate fa x))
		   
      for i below (dim fa)
      for u across x
		  
      do (incf (aref theta i) (* eta error u)))
  (enforce-bounds fa))

(defmethod clone ((fa <linear-fn-approx-with-bounds>))
  (check-exact-class fa '<linear-fn-approx-with-bounds>)
  (make-instance '<linear-fn-approx-with-bounds>
    :params (clone (params fa))
    :mins (mins fa)
    :maxs (maxs fa)
    :dim (dim fa)))

(defmethod print-object ((fa <linear-fn-approx-with-bounds>) str)
  (if *print-readably*
      (progn
	(check-exact-class fa '<linear-fn-approx-with-bounds>)
	(format str "#.(make-instance 'fn-approx::<linear-fn-approx-with-bounds> :params ~W :dim ~W :mins ~W :maxs ~W)"
		(params fa) (dim fa) (mins fa) (maxs fa)))
    (call-next-method)))

(in-package cl-user)

	
