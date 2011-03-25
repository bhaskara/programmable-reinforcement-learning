(defpackage indexed-set
  (:documentation "An <indexed-set> is a type of <numbered-set> which also includes a hashtable mapping items to their number.  Used when the item-number operation needs to be fast.

To create, use make-indexed-set.

All the standard operations on <set>s apply to <indexed-set>s.  See the set package.")
  (:use set
	utils
	common-lisp)
  (:export
   make-indexed-set))

(in-package indexed-set)
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <indexed-set> (<numbered-set>)
  ((s :type [numbered-set]
      :accessor s
      :initarg :s)
   (hta :type hta:hash-table-array
	:reader hta
	:initarg :hta
	:writer set-hta))
  (:documentation "An <indexed-set> consists of a [numbered-set] together with an #'equalp hashtable that maps set elements to their numbers.  Allows the item-number operation to be hashtable lookup, which is basically O(1). The remaining operations are forwarded to the underlying set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-indexed-set (s &key (key-fn (constantly 0)) (max-key 0) (test #'equalp))
  "make-indexed-set NUMBERED-SET &key (KEY-FN (constantly 0)) (MAX-KEY 0) (TEST #'equalp).  Make an <indexed-set>.  If the set is already an <indexed-set>, just return it.  The elements must have a canonicalize method defined.

See also documentation for <indexed-set>."
  (if (typep s '<indexed-set>)
      s
    (let ((hta (hta:make-hta key-fn max-key :test test)))
      (do-elements (x s nil i)
	(hta:set-val x hta i))
      (make-instance '<indexed-set>
	:s s :hta hta))))
	    


(defmethod clone ((s <indexed-set>))
  (make-indexed-set (s s)))

(defmethod print-object ((s <indexed-set>) str)
  (format str "<<Indexed set with ~a elements >>" (size s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations from set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod member? (x (s <indexed-set>))
  (multiple-value-bind (val present?)
      (hta:get-val x (hta s))
    (declare (ignore val))
    present?))

(defmethod item (num (s <indexed-set>))
  (item num (s s)))

(defmethod iterator ((s <indexed-set>))
  (iterator (s s)))

(defmethod size ((s <indexed-set>))
  (size (s s)))


(defmethod add ((s <indexed-set>) x &optional (pos nil))
  (assert pos () "For <indexed-set>, add must be called with POS true for now.")
  (unless (member? x s)
    (addf (s s) x pos)
    (hta:set-val x (hta s) (1- (size (s s)))))
  s)
    

(defmethod item-number (x (s <indexed-set>))
  (let ((num (hta:get-val x (hta s))))
    (aif num it (error 'item-not-in-set :item x :set s))))













