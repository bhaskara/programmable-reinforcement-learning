;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc/exp-utils.lisp
;; utilities for running experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

(defmacro avg-over-trials (n &body body)
  "avg-over-trials N &res BODY.
Run BODY N times and return an average of the results.  For this to make sense, BODY must always return something with the same structure. Currently this can be any nested combination of lists and vectors."
  (with-gensyms (v x avg)
    `(let ((,v (mapcar (lambda (,x) (declare (ignore ,x)) ,@body) (below ,n))))
       (let ((,avg (apply #'average ,v)))
	 (values ,avg (apply #'std ,avg ,v))))))

(defgeneric average (arg1 &rest args))

(defmethod average ((arg1 number) &rest args)
  (float (/ (reduce #'+ (cons arg1 args)) (1+ (length args)))))

(defmethod average ((arg1 list) &rest args)
  (apply #'mapcar #'average arg1 args))

(defmethod average ((arg1 vector) &rest args)
  (apply #'map 'vector #'average arg1 args))

(defgeneric std (avg arg1 &rest args))

(defmethod std (avg (arg1 number) &rest args)
      (sqrt
       (- (/ (loop
		 for x in (cons arg1 args)
		 sum (* x x))
	     (1+ (length args)))
	  (* avg avg))))


(defmethod std (avg (arg1 list) &rest args)
  (apply #'mapcar #'std avg arg1 args))

(defmethod std (avg (arg1 vector) &rest args)
  (apply #'map 'vector #'std avg arg1 args))
     
  