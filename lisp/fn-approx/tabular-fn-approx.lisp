(in-package fn-approx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <tabular-fn-approx> (<fn-approx>)
  ((params :type hash-table
	   :writer set-params
	   :initarg :params
	   :initform nil
	   :documentation "hash table holding parameters")
   (test :type function
	 :initform #'equalp
	 :reader test
	 :writer set-test
	 :initarg :test
	 :documentation "The test used in the hashtable")
   (default-val :reader default-val
     :documentation "value of inputs not in the table.  0 by default."
     :initform 0.0
     :initarg :default-val)
   )
		      
  
  (:documentation "Class <tabular-fn-approx>.  Initargs 
:params -  Hashtable holding the initial value of the fn.  Optional, equal to an empty table by default
:default-val - Default value for items not in the table.  Optional, 0 by default.
:test - the test used in the hashtable.  Optional, #'equalp by default.
"))

(defmethod initialize-instance :after ((fa <tabular-fn-approx>) &rest args)
  (set-params (or (params fa) (make-hash-table :test (test fa))) fa)
  (set-test (hash-table-test (params fa)) fa))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; externally called methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((fa <tabular-fn-approx>) &optional theta)
  (if theta
      (progn
	(assert (equal (type-of theta) 'hash-table) nil "~a is not a hash-table!" theta)
	(set-params theta fa))
    (set-params (make-hash-table :test (test fa)) fa))
  t)


(defmethod evaluate ((fa <tabular-fn-approx>) x)
  "if x is in the params table, use the value there, otherwise use default-val"
  (multiple-value-bind (val exists)
      (gethash x (params fa))
    (if exists
	val
      (progn
	(signal 'unknown-item-evaluate :item x :fn-approx fa)
	(default-val fa)))))


(defmethod update ((fa <tabular-fn-approx>) x y eta)
  "linearly interpolate between the old value (or default value if x is not in the table) and y"
  (let ((theta (params fa)))
    (multiple-value-bind (old-val exists)
	(gethash x theta)
      (unless exists
	(setf old-val (default-val fa))
	(signal 'unknown-item-update :item x :fn-approx fa))
      (setf (gethash x theta)
	(+ (* eta y) (* (- 1 eta) old-val)))))
  t)



(defmethod clone ((fa <tabular-fn-approx>))
  (check-exact-class fa '<tabular-fn-approx>)
  (make-instance '<tabular-fn-approx> 
    :params (clone (params fa))
    :default-val  (default-val fa)))


(defmethod print-object ((fa <tabular-fn-approx>) str)
  (if *print-readably*
      (progn
	(check-exact-class fa '<tabular-fn-approx>)
	(format str "#.(make-instance 'fn-approx::<tabular-fn-approx> :default-val ~W :params "
		(default-val fa))
	(print-hash-table-readably (params fa) str)
	(format str ")"))
    (print-unreadable-object (fa str :type t :identity t)
      (format str "with ~a entries" (hash-table-count (params fa))))))




(in-package cl-user)