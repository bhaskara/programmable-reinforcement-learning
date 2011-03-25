;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; lin-alg.lisp
;;
;; functions to do with linear algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package info
(defpackage lin-alg
  (:use common-lisp)
  (:export lp-dist dot-product))

(in-package lin-alg)


(defgeneric lp-dist (a1 a2 &optional (p 1))
  (:documentation "lp-dist A1 A2 &optional (P 1)
Return the l^p distance between two arrays of reals.  P is the norm to use. Either a positive number, or the symbol 'infinity to indicate infinity-norm"))

(defmethod lp-dist ((a1 array) (a2 array) &optional (p 1))
  
  (if (numberp p)
      (expt
       (loop
	   for i below (array-total-size a1)
	   summing (expt (abs (- (row-major-aref a1 i) (row-major-aref a2 i))) p))
       (/ 1 p))
    (loop
	for i below (array-total-size a1)
	maximizing (abs (- (row-major-aref a1 i) (row-major-aref a2 i))))))


;; lp-dist
;; return the l^p distance between two hashes whose values are real.
;;
;; REQUIRED
;; h1, h2 : the two hashes
;;
;; OPTIONAL
;; p : the norm to use.  1 by default.
;;
;; if a key occurs in h1 but not h2 (or vice versa) its value in h2 is
;; assumed to be 0.
(defmethod lp-dist ((h1 hash-table) (h2 hash-table) &optional (p 1))

  (expt
   (+
    
    ; first loop over elements in first hash
    (loop
	for k being each hash-key of h1
	summing (let ((v (gethash k h1))
		      (v2 (gethash k h2)))
		  (expt (abs (- v (if v2 v2 0))) p)))
    
    ; then loop over those elements that are in the second hash but not the first
    (loop
	for k being each hash-key of h2
	summing (let ((v (gethash k h1)))
		  (if v
		      0
		    (expt (abs (gethash k h2)) p)))))
   (/ 1 p)))

;; dot-product
;; return the dot product of two vectors
(defmethod dot-product ((a vector) (b vector))
  (loop
    for x across a and y across b
    summing (* x y)))

;; dot-product
;; return the dot product of two lists
(defmethod dot-product ((a list) (b list))
  (loop
    for x in a and y in b
    summing (* x y)))



(in-package cl-user)


