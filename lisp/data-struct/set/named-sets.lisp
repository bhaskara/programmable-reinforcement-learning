;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/set/named-sets.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package set)


(defmethod is-empty ((s symbol))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; natural numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod member? (item (s (eql 'natural-numbers)))
  (and (typep item 'integer)
       (>= item 0)))

(defmethod iterator ((s (eql 'natural-numbers)))
  (let ((current 0))
    (lambda ()
      (let ((ret current))
	(incf current)
	(iterator-not-done ret)))))

(defmethod size ((s (eql 'natural-numbers)))
  "TODO is this the right thing to do?"
  'aleph-null)

(defmethod item-number (item (s (eql 'natural-numbers)))
  (if (member? item s)
      item
    (error 'item-not-in-set :item item :set s)))
  


(defmethod item (num (s (eql 'natural-numbers)))
  "Todo condition"
  num)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; real numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod member? (item (s (eql 'real-numbers)))
  (typep item 'real))

(defmethod size ((s (eql 'real-numbers)))
  "TODO is this the right thing to do?"
  'aleph-one)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; universal set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod member? (item (s (eql 'universal-set)))
  (declare (ignore item))
  t)


