;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/set-hash-set.lisp
;; treating hash tables as sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package set)

(defmethod size ((s hash-table))
  (hash-table-count s))

(defmethod item-number (item (s hash-table))
  (multiple-value-bind (num present?)
      (gethash item s)
    (if present?
	num
      (error 'item-not-in-set :item item :set s))))

(defmethod member? (item (s hash-table))
  (multiple-value-bind (num present?)
      (gethash item s)
    (declare (ignore item))
    present?))


(defmethod add ((s hash-table) item &optional pos)
  (declare (ignore pos))
  (unless (hash-table-has-key s item)
    (setf (gethash item s) (hash-table-count s)))
  s)



    