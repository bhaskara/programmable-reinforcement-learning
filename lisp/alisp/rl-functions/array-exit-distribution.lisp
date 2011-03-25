(in-package aed)

(defclass <array-exit-distribution> (<cond-prob-dist>)
  ((featurizer :reader featurizer :initarg :featurizer :type function
	       :initform (lambda (omega u) (cons (canonicalize omega) (canonicalize u))))
   (key-fn :reader key-fn :type function :initarg :key-fn)
   (cond-dists :type (simple-array * 1) :reader cond-dists :writer set-cond-dists :initarg :cond-dists))
  (:documentation "A conditional probability distribution for exit distributions in ALisp.

Initargs
:featurizer - Function of two arguments that maps from omega and u to a feature vector that is passed to fn-approx, which returns a probability distribution over exit state.  Default is just (canonicalize (cons omega u))
:key-fn - Function from omega, u to a nonnegative integer
:Num-keys - key-fn takes values between 0 and num-keys - 1
"))

(defmethod initialize-instance :after ((d <array-exit-distribution>) &rest args &key key-fn num-keys)
  (assert (and key-fn num-keys))
  (unless (slot-boundp d 'cond-dists)
    (set-cond-dists (make-array num-keys :initial-element nil) d)))


(defmethod cond-dist ((p <array-exit-distribution>) x)
  (exit-dist p (car x) (cdr x)))

(defmethod exit-dist ((p <array-exit-distribution>) omega u)
  (let* ((v (funcall (featurizer p) omega u))
	 (ind (funcall (key-fn p) v)))
    (assert ind () "Invalid item ~a given to array-exit-dist ~a" v p)
    (let ((d (aref (cond-dists p) ind)))
      (renormalize d))))

(defmethod clone ((pe <array-exit-distribution>))
  (make-instance '<array-exit-distribution> :featurizer (featurizer pe)
		 :cond-dists (clone (cond-dists pe)) :key-fn (key-fn pe)))

(defmethod update-exit-dist ((p <array-exit-distribution>) omega u new-dist eta)
  (let ((d (make-deterministic-dist 'dummy-unknown-state)))
    (unless (equal d new-dist)
      (let* ((features (funcall (featurizer p) omega u))
	     (ind (funcall (key-fn p) features))
	     (cd (cond-dists p)))
	(assert ind () "Invalid item ~a given to array exit dist ~a" ind features)
	(let ((dist (aref cd ind)))
	  (setf (aref cd ind)
	    (if dist 
		(updatef dist new-dist eta)
	      (updatef d new-dist eta))))))))

