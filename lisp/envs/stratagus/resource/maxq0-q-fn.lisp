(defpackage maxq0-q-function
  (:documentation
   "Package for defining MAXQ0 Q-functions and associated feature templates for concurrent ALisp programs.
Types
-----
<maxq0-q-function>
") 
  (:use
   utils
   calisp)
  (:export
   <maxq0-q-function>))

(in-package maxq0-q-function)

(defclass <maxq0-q-function> (<calisp-approx-q-function>)
  ((linear-fn-approx :initform (make-instance 'fn-approx:<linear-fn-approx>))   
   (tabular-fn-approx :initform (make-instance 'fn-approx:<tabular-fn-approx>))))

(defmethod update ((q-fn <maxq0-q-function>) omega u target eta)
    (dolist (tid (get-thread-ids omega))
      (update-linear-or-tabular (fn-approx q-fn) (featurize q-fn omega u tid) target eta)))

(defmethod update-tid ((q-fn <maxq0-q-function>) omega u target eta tid)
  (update-linear-or-tabular (fn-approx q-fn) (featurize q-fn omega u tid) target eta))

(defmethod clone ((q-fn <maxq0-q-function>))
  (make-instance '<maxq0-q-function> :fn-approx (clone (fn-approx q-fn))))

(defmethod evaluate ((q-fn <maxq0-q-function>) omega u)
  (let ((sum 0))
    (dolist (tid (get-thread-ids omega))
      (let ((subtask (get-subtask tid omega)))
	(setf sum (+ sum (car (evaluate-max-node omega tid subtask))))))))
	  
      (evaluate-linear-or-tabular (fn-approx q-fn) (featurize q-fn omega u tid)))

(defmethod evaluate-tid ((q-fn <maxq0-q-function>) omega u tid)
  (evaluate-linear-or-tabular (fn-approx q-fn) (featurize q-fn omega u tid)))

(defmethod featurize ((q-fn <maxq0-q-function>) omega u thread-id)
  (funcall (featurizer q-fn) omega (get-choice u thread-id) thread-id))

(defun get-thread-ids (omega)
  (let ((list-of-ids nil))
    (maphash (lambda(k v) (setf list-of-ids (append list-of-ids (list k)))) (js-thread-states omega)))
  list-of-ids)

(defun get-choice (u tid)
  (second (assoc-if #'(lambda(x) (= tid x)) u)))

(defun evaluate-max-node (omega tid subtask)
  (if (primitive-subtask? subtask)
      (cons (evaluate-linear-or-tabular (fn-approx q-fn) (featurize q-fn omega nil tid)) subtask)
      (let ((children-subtasks (get-children-subtasks subtask)))
        (let ((max-so-far (cons 0 nil)))
	  (dolist (j children-subtasks)
	    (let* ((recursive-evaluate (evaluate-max-node omega tid j))
	           (j-value (+ (car (recursive-evaluate)) (evaluate-linear-or-tabular (fn-approx q-fn) (featurize q-fn omega 
		     (acons tid j '()) tid)))))
	      (if (> j-value (car max-so-far))
		  (setf max-so-far (cons j-value j))))))
	  max-so-far)))
	
(defun get-subtask (tid omega)
)

;; these are references into the hash table

(defun get-children-subtasks (subtask)
)

(defun primitive-subtask? (subtask)
)

;; TODO: Implement Featurizer, inform-env-step, and inform-end-choice-block

(defmethod update-linear-or-tabular ((q-fn <maxq0-q-function>) features target eta)
    (if (equalp (car features) 'linear)
	(fn-approx:update (linear-fn-approx q-fn) (cdr features) target eta)
        (fn-approx:update (tabular-fn-approx q-fn) (cdr features) target eta)))

(defmethod evaluate-linear-or-tabular ((q-fn <maxq0-q-function) features)
    (if (equalp (car features) 'linear)
	(fn-approx:evaluate (linear-fn-approx q-fn) (cdr features))
        (fn-approx:evaluate (tabular-fn-approx q-fn) (cdr features))))