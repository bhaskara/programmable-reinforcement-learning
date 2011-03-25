;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/set/number-set.lisp
;; code relating to the integer n viewed as the set (0,1,...,n-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package set)

(defmethod member? (item (s integer))
  (and (typep item 'integer) (utils:between2 item 0 s)))

(defmethod iterator ((s integer))
  (let ((current 0))
    (lambda ()
      (if (< current s)
	  (let ((ret current))
	    (incf current)
	    (iterator-not-done ret))
	(iterator-done)))))

(defmethod size ((s integer))
  s)

(defmethod item-number (item (s integer))
  (if (member? item s)
      item
    (error 'item-not-in-set :item item :set s)))

(defmethod item (num (s integer))
  "TODO check bounds, signal condition"
  (when (member? num s)
    num))


(defmethod subset ((s1 integer) (s2 integer))
  (<= s1 s2))