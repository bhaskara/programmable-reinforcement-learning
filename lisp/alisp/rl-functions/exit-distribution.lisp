(defpackage alisp-exit-distribution
  (:documentation "Package for exit distributions in ALisp.

Types
-----
<hash-exit-distribution>
<array-exit-distribution>

Operations
----------
update-exit-dist
exit-dist
Operations for <cond-prob-dist>
")

  (:nicknames aed)
  (:export
   <hash-exit-distribution>
   <array-exit-distribution>
   update-exit-dist
   exit-dist)
  (:use
   cl
   prob
   utils))
   
(in-package aed)

(defclass <hash-exit-distribution> (<cond-prob-dist>)
  ((featurizer :reader featurizer :initarg :featurizer :type function
	       :initform (lambda (omega u) (cons (canonicalize omega) (canonicalize u))))
   (cond-dists :type hash-table :reader cond-dists :initform (make-hash-table :test #'equalp)
	       :initarg :cond-dists))
  (:documentation "A conditional probability distribution for exit distributions in ALisp.

Initargs
:featurizer - Function of two arguments that maps from omega and u to a feature vector that is passed to fn-approx, which returns a probability distribution over exit state.  Default is just (canonicalize (cons omega u))
"))



(defmethod cond-dist ((p <hash-exit-distribution>) x)
  (exit-dist p (car x) (cdr x)))

(defgeneric exit-dist (p omega u)
  (:documentation "exit-dist P OMEGA U")
  (:method ((p <hash-exit-distribution>) omega u)
	   (multiple-value-bind (dist present?)
	       (gethash (funcall (featurizer p) omega u) (cond-dists p))
	     (if present?
		 ;; if omega, u is in the table, renormalize dist to remove the dummy unknown state
		 (renormalize dist)
	       ;; otherwise, just return a deterministic dist over unknown-state
	       (make-deterministic-dist 'dummy-unknown-state)))))


(defmethod clone ((pe <hash-exit-distribution>))
  (make-instance '<hash-exit-distribution> :featurizer (featurizer pe)
		 :cond-dists 
		 (copy-hash-table (cond-dists pe) :value-copy-fn #'clone-dist)))


(defgeneric update-exit-dist (exit-dist omega u new-dist eta)
  (:documentation  "update-exit-dist EXIT-DIST OMEGA U NEW-DIST ETA.  Move the exit distribution at OMEGA,U closer to NEW-DIST by ETA, unless NEW-DIST is unknown.  If OMEGA,U hasn't been seen before, pretend that its current distribution assigns probability 1 to 'dummy-unknown-state.")
  (:method ((exit-dist <hash-exit-distribution>) omega u new-dist eta)
  
	   (let ((d (make-deterministic-dist 'dummy-unknown-state)))
	     (unless (equal d new-dist)
	       (let ((features (funcall (featurizer exit-dist) omega u))
		     (table (cond-dists exit-dist)))
		 (if (hash-table-has-key table features)
		     (updatef (gethash features table) new-dist eta)
		   (setf (gethash features table)
		     (updatef d new-dist eta))))))))



(defun renormalize (dist)
  "renormalize a distribution to get rid of the unknown state, unless that's the only state.  Note this implicitly relies on the distribution (which originally comes from make-deterministic-dist or update-dist) being a list."
  (let ((total-prob (- 1 (cdr (assoc 'dummy-unknown-state dist)))))
    (if (> total-prob 0)
	(loop
	    for x in dist
	    unless (eq (car x) 'dummy-unknown-state)
	    collect (cons (car x) (/ (cdr x) total-prob)))
      dist)))


