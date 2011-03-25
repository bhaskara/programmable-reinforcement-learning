;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-struct/set/create-sets.lisp
;; Ways of creating new sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; composing a function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <image-set> (<numbered-set>)
  ((f :initarg :f
      :reader f
      :type function)
   (f-inv :initarg :f-inv
	  :reader f-inv
	  :initform nil
	  :type function)
   (s :initarg :s
      :reader s
      :type [set])))

(defun make-image-set (s f &optional (f-inv nil))
  "make-image-set SET FUNCTION &optional (F-INV nil).  Returns a new set which is the image of SET under FUNCTION.  The new set will not be stored in memory; rather, its members will be computed as needed by applying FUNCTION to elements of SET.  F-INV, if provided, must be the inverse of FUNCTION.  It is optional, but the operations may be slower if it is not provided."
  (make-instance '<image-set> :s s :f f :f-inv f-inv))

(defmethod member? (item (is <image-set>))
  (let ((s (s is)))
    (aif (f-inv is)    
	(member? (funcall it item) s)
      (do-elements (x s nil)
	(when (same x item)
	  (return t))))))
  
(defmethod iterator ((is <image-set>))
  (let ((iter (iterator (s is)))
	(f (f is)))
    (lambda ()
      (multiple-value-bind (item done?)
	  (funcall iter)
	(if done?
	    (values nil t)
	  (values (funcall f item) nil))))))

(defmethod item-number (item (is <image-set>))
  (let ((s (s is)))
    (aif (f-inv is)
	(item-number (funcall (f-inv is) item) (s is))
      (progn
	(do-elements (x s nil i)
	  (when (same x item)
	    (return-from item-number i)))
	(error 'item-not-in-set :item item :set is)))))

(defmethod item (num (is <image-set>))
  (funcall (f is) (item num (s is))))

(defmethod size ((is <image-set>))
  (size (s is)))



      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursive closure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recursive-closure (init-set successor-fn &key (test #'equalp) (print-progress nil) 
						     (key-fn (constantly 0)) (max-key 0))
  "recursive-closure INIT-SET SUCCESSOR-FN &key (TEST #'equalp) (PRINT-PROGRESS nil) (KEY-FN (constantly 0)) (MAX-KEY 0)

Compute the recursive closure of INIT-SET under SUCCESSOR-FN.  Returns the set (as an <indexed-set> over a vector).

PRINT-PROGRESS : print a '.' every so-many new elements.
KEY-FN : function that takes in an element and outputs a nonnegative integer.  Used when we have prior knowledge about a good hash function for the items.
MAX-KEY : must be provided if KEY-FN is provided.  The max value that KEY-FN can take."
  
  (let ((v (make-array 0 :adjustable t :fill-pointer 0))
	(hta (hta:make-hta key-fn max-key :test test))
	(ind 0))
    
    (do-elements (x init-set)
      (vector-push-extend x v)
      (hta:set-val x hta t))
    
    (while (< ind (length v))
      (let ((x (aref v ind)))
	(do-elements (y (funcall successor-fn x))
	  (unless (hta:get-val y hta)
	    (vector-push-extend y v)
	    (hta:set-val y hta t)
	    
	    (when (and print-progress (= 0 (mod (length v) print-progress)))
	      (format t ".")))))
      (incf ind))
    
    (when print-progress (format t "."))
    (indexed-set:make-indexed-set v :key-fn key-fn :max-key max-key)))
	
      
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerset of a set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <powerset> (<set>)
  ((base-set :initarg :base-set :reader base-set)))
  
(defun powerset (s)
  "powerset S.  Return the set of all subsets of S.  The powerset is not stored explicitly.  The only supported operations are size,  member?, clone, and print-object."
  (make-instance '<powerset> :base-set s))

(defmethod size ((s <powerset>))
  (expt 2 (size (base-set s))))

(defmethod member? (x (s <powerset>))
  (subset x (base-set s)))

(defmethod clone (s)
  s)

(defmethod print-object ((s <powerset>) str)
  (if *print-readably*
      (format str "#.(powerset ~W)" (base-set s))
    (print-unreadable-object (s str :type t)
      (format str "of ~W" (base-set s)))))