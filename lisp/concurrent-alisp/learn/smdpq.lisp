(defpackage calisp-smdpq
  (:nicknames csq)
  (:documentation "Package calisp-smdpq.  The smdp q-learning algorithm, applied to concurrent alisp. 

Types
<smdpq>

See also calisp-user package for other operations on <calisp-learning-algorithm> objects
")
  (:export <smdpq>)
  (:use common-lisp
	utils
	policy
	calisp-obs))


(in-package calisp-smdpq)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <smdpq> (<calisp-learning-algorithm> <q-learning-algorithm>)
  ((prev-omega :accessor prev-omega)
   (prev-u :accessor prev-u)
   (total-reward :accessor rew)
   (q-function :reader q-fn :type <calisp-q-function> :initarg :q-fn :writer set-q-fn
	       :initform (make-instance 'calisp-obs:<calisp-approx-q-function>
			   :fn-approx (make-instance 'fn-approx:<tabular-fn-approx>)))
   (init-q-function :reader init-q-fn :type <calisp-q-function> :writer set-init-q-fn)
   (discount :reader disc :initarg :disc :initform 1.0)
   (total-discount :accessor total-disc)
   (learning-rate :reader lrate :initarg :lrate :initform .02))
    
  (:documentation "Class <smdpq>.  Subclass of <calisp-learning-algorithm> that uses SMDP Q-learning.

Initargs
:disc discount.  1.0 by default.
:lrate learning rate
:q-fn q-function
"))

(defmethod initialize-instance :after ((alg <smdpq>) &rest args &key (featurizer nil))
  (declare (ignore args))
  (when featurizer
    (set-q-fn (make-instance 'calisp-obs:<calisp-approx-q-function>
		:fn-approx (make-instance 'fn-approx:<tabular-fn-approx>)
		:featurizer featurizer)
	      alg))
  (set-init-q-fn (clone (q-fn alg)) alg))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods from <calisp-learning-algorithm>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod reset ((alg <smdpq>))
  (set-q-fn (clone (init-q-fn alg)) alg))


(defmethod inform-start-episode ((alg <smdpq>) s)
  (declare (ignore s))
  (setf (prev-omega alg) nil
	(prev-u alg) nil
	(rew alg) 0
	(total-disc alg) 1.0))
  
  
(defun smdpq-finish-episode (alg)
  ;; do a final backup if possible
  (let ((prev-omega (prev-omega alg))
	(prev-u (prev-u alg)))
    (when prev-omega
      (debug-msg alg "~&Backup towards state ~a and choice ~a towards ~a." 
		 prev-omega prev-u (rew alg))
      
      (q-fn:update (q-fn alg) prev-omega prev-u (rew alg)
		   (lrate:get-rate (lrate alg) (cons prev-omega prev-u)))
      (setf (prev-omega alg) nil
	    (prev-u alg) nil))))
   
(defmethod inform-env-step ((alg <smdpq>) act rew to term)
  (declare (ignore to act))
  ;; accumulate reward
  (incf (rew alg) (* (total-disc alg) rew))
  (multf (total-disc alg) (disc alg))
  (when term (smdpq-finish-episode alg)))


(defmethod inform-calisp-step ((alg <smdpq>) omega u)
  ;; when possible, do a backup
  
  (let ((prev-omega (prev-omega alg))
	(prev-u (prev-u alg))
	(q-fn (q-fn alg)))
    
    
    
    
    
    (when prev-omega
      (let ((new-val (+ (rew alg)
			(* (total-disc alg)
			   (handler-case
			       (q-fn:value q-fn omega)
			     (q-fn:unknown-state-action () 0))))))

	(debug-msg alg "~&Backup towards state ~a choice ~a towards ~a"
		   prev-omega prev-u new-val)
	(q-fn:update q-fn prev-omega prev-u new-val
		     (lrate:get-rate (lrate alg) (cons prev-omega prev-u))))
      ))

  (setf (prev-omega alg) omega
	(prev-u alg) u
	(rew alg) 0
	(total-disc alg) 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; knowledge state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knowledge-state ((alg <smdpq>) &optional (fresh t))
  (let ((q (q-fn alg)))
    (if fresh (clone q) q)))


(defmethod get-q-fn ((alg <smdpq>) ks)
  ks)




