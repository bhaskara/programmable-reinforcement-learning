;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prob/vector-probability-distribution
;; Code for treating vectors as probability distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package prob)


(defmethod sample-space ((v vector))
  (length v))

(defmethod sample ((v vector))
  (loop
      with n = (length v)
	       
      with r = (random 1.0)
      with cumsum = 0
      for i below n
      for y across v
		  
      do (incf cumsum y)
      until (< r cumsum)
      finally (return (min i (1- n)))))

(defmethod prob ((v vector) x)
  (if (and (integerp x) (utils:between x 0 (1- (length v))))
      (aref v x)
    (progn
      (signal 'element-not-in-domain)
      0)))

(defmethod expectation ((v vector) rv)
  (loop
      for x across v
      for i from 0
      summing (* x (evaluate-rv rv i))))



(defmethod is-valid-prob-dist ((v vector))
  (and (every (lambda (x) (>= x 0)) v)
       (< (abs (- (reduce #'+ v) 1)) *prob-dist-sum-tol*)))

      