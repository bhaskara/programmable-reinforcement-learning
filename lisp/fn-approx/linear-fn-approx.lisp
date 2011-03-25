(in-package fn-approx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition of <linear-fn-approx> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <linear-fn-approx> (<fn-approx>)
  ((params :type vector)
   (dim :type fixnum
	:initarg :dim
	:reader dim))
  (:documentation "Class for linear function approximation

Initialization arguments :
:dim - dimension of parameter vector.  
:params - initial parameters.  optional, if provided must be a vector of dimension dim, defaults to #(0 0 .... 0)"))



(defmethod initialize-instance :after ((fa <linear-fn-approx>) 
				       &rest initargs)
  
  (declare (ignore initargs))
  (let ((unbound (not (slot-boundp fa 'params))))
    (assert (or unbound (= (length (params fa)) (dim fa))) nil
      "Dimension of ~a is not ~a" (params fa) (dim fa))
    (when unbound
      (set-params (make-array (dim fa) :initial-element 0.0) fa))))
     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; externally called methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((fa <linear-fn-approx>) &optional theta)
  (if theta
      (progn
	(assert (equal (length theta) (dim fa)) nil "Dimension of ~a is not ~a" theta (dim fa))
	(set-params theta fa))
    (set-params (make-array (dim fa) :initial-element 0.0) fa))
  t)


(defmethod evaluate ((fa <linear-fn-approx>) x)
  (loop
      for u across x
      for v across (params fa)
      sum (* u v)))


(defmethod update ((fa <linear-fn-approx>) x y eta)
  "take a step in the direction of the negative gradient of the squared error given this example"
  (loop
      with theta = (params fa)
      with error = (- y (evaluate fa x))
		   
      for i below (dim fa)
      for u across x
		  
      do (incf (aref theta i) (* eta error u))))

(defmethod clone ((fa <linear-fn-approx>))
  (check-exact-class fa '<linear-fn-approx>)
  (make-instance '<linear-fn-approx>
    :params (clone (params fa))
    :dim (dim fa)))

(defmethod print-object ((fa <linear-fn-approx>) str)
  (if *print-readably*
      (progn
	(check-exact-class fa '<linear-fn-approx>)
	(format str "#.(make-instance 'fn-approx::<linear-fn-approx> :params ~W :dim ~W)"
		(params fa) (dim fa)))
    (print-unreadable-object (fa str :type t :identity nil)
      (format str "with weight-vector ~W" (params fa)))))

(in-package cl-user)

	
