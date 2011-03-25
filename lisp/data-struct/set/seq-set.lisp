;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/set/seq-set.lisp
;; code relating to sequences viewed as sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package set)

(defmethod member? (item (s sequence))
  (some (lambda (x) (same x item)) s))

(defmethod iterator ((s list))
  (let ((current s))
    (lambda ()
      (if current
	  (let ((next (first current))
		(remaining (rest current)))
	    (setf current remaining)
	    (iterator-not-done next))
	(iterator-done)))))

(defmethod iterator ((s vector))
  (let ((ind 0))
    (lambda ()
      (if (< ind (length s))
	  (let ((next (aref s ind)))
	    (incf ind)
	    (iterator-not-done next))
	(iterator-done)))))


(defmethod size ((s sequence))
  (length s))

(defmethod item-number (item (s sequence))
  (let ((pos (position item s :test #'same)))
    (aif pos it (error 'item-not-in-set :item item :set s))))
	  

(defmethod item (num (s sequence))
  (elt s num))
  


(defmethod add ((s vector) item &optional (pos nil))
  (declare (ignore pos))
  (if (member? item s)
      s
    (if (and (adjustable-array-p s)
	     (array-has-fill-pointer-p s))
	(progn
	  (vector-push-extend item s)
	  s)
      (let ((new-s (make-array (1+ (length s))
			       :adjustable t
			       :fill-pointer 0)))
	(map nil
	  (lambda (x) (vector-push x new-s))
	  s)
	(vector-push item new-s)
	new-s))))

(defmethod add ((s list) item &optional (pos nil))
  (declare (ignore pos))
  (if (member? item s)
      s
    (nconc s (list item))))

(defmethod destructive-union ((s list) (s2 list))
  (nconc s s2))


   
   
   
(defmethod is-empty ((s list))
  (null s))

