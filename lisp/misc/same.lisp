;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc/same.lisp
;; Defines the generic function same, for telling if two objects are equal.  
;; Intended to work for compound objects, unlike equalp
;; TODO : the use of same needs to be reconsidered. In most situations,
;; equalp is good enough.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

(defgeneric same (x y)
  (:documentation "generic function same X Y.  Return T if X and Y are the 'same'.  For numbers, strings, and symbols, just calls equal.  For arrays and cons, recursively descends.  For all other types, calls equalp by default.  Can define methods for other compound object types.")
  (:method ((x number) (y number)) (eql x y))
  (:method ((x symbol) (y symbol)) (eq x y))
  (:method ((x string) (y string)) (equal x y))
  (:method ((x t) (y t)) (equalp x y))
  (:method ((x cons) (y cons)) (and (same (car x) (car y)) (same (cdr x) (cdr y))))
  (:method ((a1 array) (a2 array))
	   (and (equal (array-dimensions a1) (array-dimensions a2))
		(loop
		    for i below (array-total-size a1)
		    if (not (same (row-major-aref a1 i) (row-major-aref a2 i)))
		    do (return nil)
		    finally (return t)))))
  

	   