(in-package fn-approx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <array-fn-approx> (<fn-approx>)
  ((params :type array :reader params :writer set-params :initarg :params)
   (key-fn :type function :reader key-fn :initarg :key-fn))
  (:documentation "Class <array-fn-approx>.  A particular implementation of tabular function approximation.

Initargs
:key-fn - a function from items to nonnegative integers.  Should return nil for items that are illegal.
:num-keys - keys must take values between 0 and num-keys - 1 can take. "))

(defmethod initialize-instance :after ((fa <array-fn-approx>) &rest args &key key-fn num-keys)
  (assert (and key-fn num-keys))
  (unless (slot-boundp fa 'params)
    (set-params (make-array num-keys :element-type 'float :initial-element nil) fa)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; externally called methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((fa <array-fn-approx>) &optional theta)
  (if theta
      (set-params (clone theta) fa)
    (fill (params fa) nil)))

(defmethod evaluate ((fa <array-fn-approx>) x)
  (let ((ind (get-item-key fa x)))
    (assert ind () "Unknown item ~a given to array fn approx ~a" x fa)
    (let ((val (aref (params fa) ind)))
      (unless val
	(signal 'unknown-item-evaluate :item x :fn-approx fa))
      val)))

(defmethod update ((fa <array-fn-approx>) x y eta)
  (let ((ind (get-item-key fa x)))
    (assert ind () "Unknown item ~a given to array fn approx ~a" x fa)
    (let ((old-val (evaluate fa x)))
      (unless old-val (signal 'unknown-item-update :item x :fn-approx fa))
      (setf (aref (params fa) ind)
	(+ (* eta y) (* (- 1 eta) (or old-val 0)))))))

(defmethod clone ((fa <array-fn-approx>))
  (make-instance '<array-fn-approx> :params (clone (params fa))
		 :key-fn (key-fn fa)))


(defun get-item-key (fa x)
  (funcall (key-fn fa) x))



	   

(in-package cl-user)

	
