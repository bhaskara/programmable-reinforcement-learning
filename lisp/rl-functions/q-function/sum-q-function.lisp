(in-package q-function)

(defclass <sum-q-function> (<q-function>)
  ((q-fns :type list
	  :reader q-fns
	  :initarg :q-fns)
   (strict :type boolean
	   :reader strict
	   :writer set-strict
	   :initform t))
  (:documentation "Class <sum-q-function>.  Initargs
:q-fns - list of q-functions which are added together to make this one

Other accessors
strict, set-strict - if strictness is t (by default), then verifies, whenever choices is called, that all the q-functions really have the same list of choices (takes time O(num-q-functions*num-choices))"))



(defmethod sum-q-functions ((q-fn <q-function>) &rest q-fns)
  "Q-functions can be added to make new ones"
  (make-instance '<sum-q-function> :q-fns (cons q-fn q-fns)))



(defmethod evaluate ((q-fn <sum-q-function>) omega u)
  (handler-case
      (loop for q in (q-fns q-fn) summing (evaluate q omega u))
    (unknown-state-action ()
      (error 'unknown-state-action :state omega :action u :q-fn q-fn))))

(defmethod reset ((q-fn <sum-q-function>) &optional (params nil params-supplied))
  (declare (ignore params))
  (assert (not params-supplied) nil "Initial parameters should not be supplied when calling reset on <sum-q-function> object.")
  (dolist (q (q-fns q-fn))
    (reset q)))

(defmethod update ((q-fn <sum-q-function>) omega u target eta)
  (declare (ignore omega u target eta))
  (assert nil nil "Update method can't be called for <sum-q-function> object."))

(defmethod clone ((q-fn <sum-q-function>))
  (check-exact-class q-fn '<sum-q-function>)
  (make-instance '<sum-q-function> :q-fns (clone (q-fns q-fn))))

(defmethod print-object ((q-fn <sum-q-function>) str)
  (if *print-readably*
      (progn
	(check-exact-class q-fn '<sum-q-function>)
	(format str "#.(make-instance 'q-fn:<sum-q-function> :q-fns (list ")
	(dolist (q (q-fns q-fn))
	  (write q :stream str))
	(format str "))"))
    (print-unreadable-object (q-fn str :type t :identity nil)
      (format str "with ~W components" (length (q-fns q-fn))))))



(defmethod choices ((q-fn <sum-q-function>) omega)
  (let* ((q-fns (q-fns q-fn))
	 (first-q-choices (choices (first q-fns) omega)))
    
    ;; a time-consuming assert that should maybe be removed eventually
    (assert (or (not (strict q-fn)) (every (lambda (q) (equal (choices q omega) first-q-choices)) (rest q-fns))) nil
      "When q-functions are being summed, they must have the same list of choices at each state.  But at state ~a, the q-functions had choice lists ~a."
      omega (mapcar (lambda (x) (choices x omega)) q-fns))
    first-q-choices))
    
(defgeneric evaluate-comps (sum-q-fn omega u)
  (:documentation "Return list containing value of each q-function evaluated at omega and u")
  (:method ((q-fn <sum-q-function>) omega u)
	   (loop for q in (q-fns q-fn) collecting (evaluate q omega u))))

(defmethod print-advice ((adv q-fn:<sum-q-function>) omega choices str)
  (handler-bind ((q-fn:unknown-state-action 
		  #'(lambda (c)
		      (declare (ignore c))
		      (use-value 'unknown))))
    (format str "~&Componentwise Q-values are ~a"
	    (let ((q-vals (make-array 0 :fill-pointer 0 :adjustable t)))
	    
	      (set:do-elements (u choices q-vals)
		(vector-push-extend (cons u 
					  (mapcar #'list '(Q Qr Qc Qe)
						  (cons
						   (round-decimal (q-fn:evaluate adv omega u) 2)
						   (mapcar (lambda (x) (round-decimal x 2))
							   (q-fn:evaluate-comps adv omega u)))))
				    q-vals))))
    (handler-case (format str "~&Recommended choice is ~a" (q-fn:best-choice adv omega))
      (q-fn:unknown-state-action () (format str "~&Unable to determine best choice.")))))


