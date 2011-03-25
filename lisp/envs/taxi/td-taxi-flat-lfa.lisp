(defpackage td-taxi-flat-lfa
  (:documentation "td-taxi-flat-lfa.  Defines a simple linear approximation architecture for the flat Q-function in the taxi domain.")
  (:use td-taxi-env
	cl
	grid-world
	q-function
	utils)
  (:export make-taxi-featurizer)
  )

(in-package td-taxi-flat-lfa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Featurizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-taxi-featurizer (e)
  (flet ((source-dist (s a)
	   (shortest-path-dist e (result-legal e (taxi-pos s) a) (pass-source s)))
	 (dest-dist (s a)
	   (shortest-path-dist e (result-legal e (taxi-pos s) a) (pass-dest s))))
    (make-q-featurizer
     (constantly 1)
     #'will-have-pass
     #'will-dropoff-pass
     (fn (* (indicator have-pass?) source-dist) 2)
     (fn (* (indicator have-pass?) dest-dist) 2)
     (fn (* (indicator (not have-pass?)) source-dist) 2)
     (fn (* (indicator (not have-pass?)) dest-dist) 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun have-pass? (s a)
  (declare (ignore a))
  (eql (pass-loc s) 'in-taxi))

(defun will-have-pass (s a)
  (let ((ploc (pass-loc s)))
    (indicator
     (or (eql ploc 'in-taxi)
	 (eql ploc 'at-dest)
	 (and (equal (taxi-pos s) (pass-source s))
	      (eql ploc 'at-source)
	      (eql a 'P))))))

(defun will-dropoff-pass (s a)
  (indicator
   (or (eql (pass-loc s) 'at-dest)
       (and (equal (taxi-pos s) (pass-dest s))
	    (eql (pass-loc s) 'in-taxi)
	    (eql a 'D)))))
