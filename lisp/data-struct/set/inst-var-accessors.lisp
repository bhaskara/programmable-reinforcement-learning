(defpackage instantiation-variable-accessors
  (:documentation "Package that provides various functions/macros to create accessors for an instantiation with variables.  

The basic set up is that we have some datatype that represents 'instantiations' to a known set of variables.  We want to refer to each variable using a number.  We need functions to create new instantiations, and get and set variable values in an instantiation either by number or some other id.

An instantiation accessor encapsulates all the above operations.  The most general type of instantiation accessor can be created using make-inst-var-accessors and specifying functions for each operation.  

In addition, there are some standard ways of collecting groups of variables into single objects, e.g. vectors.  The operations make-vec-accessors, make-list-accessors, make-single-object-accessors, and make-struct-accessor automate the process of creating accessors for these common cases.

Given an instantiation accessor, the create-inst, get/set-var-val, and get/set-var-val-by-name functions can be used together with this object to access the variables in an instantiation.  They may return 'illegal-instantiation if given something which isn't actually a valid instantiation for the accessor.

One restriction is that the symbol 'uninstantiated in this package refers to an uninstantiated variable.  So actually having that be a possible legal value of a variable would lead to problems.  

Accessing instantiations and variables
--------------------------------------
create-inst
get-var-val
get-var-val-by-name
set-var-val
set-var-val-by-name
get-var-num
detect-invalid-inst
var-names
num-vars

Creating instantiation accessors
--------------------------------
make-vec-accessors
make-list-accessors
make-alist-accessors
make-single-object-accessors
make-struct-accessors
make-inst-var-accessors

Other
-----
single-object
illegal-instantiation
")
  (:use
   cl
   set
   utils)
  (:nicknames inst-vars)
  (:export
   uninstantiated
   make-inst-var-accessors
   make-vec-accessors
   make-list-accessors
   make-alist-accessors
   make-struct-accessors
   make-single-object-accessors
   get-var-val
   create-inst
   set-var-val
   get-var-val-by-name
   set-var-val-by-name
   get-var-num
   detect-invalid-inst
   var-names
   num-vars
   single-object
   illegal-instantiation))

(in-package inst-vars)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic type - TODO redo this like with alists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (inst-var-accessors (:conc-name acc-))
  creator
  readers
  writers
  invalid-inst-detector
  var-num
  var-names)

(intern 'uninstantiated)

(defmethod print-object ((x inst-var-accessors) str)
  (format str "<< Instantiation variable accessors >>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessing instantiations and variable values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-inst (a)
  (:documentation "create-inst INST-ACC.  Create a new instantiation of the type determined by INST-ACC.  The return value should not share structure with any other object.")
  (:method ((a inst-var-accessors))
	   (funcall (acc-creator a))))

(defgeneric get-var-val (a x i)
  (:documentation "get-var-val INST-ACC INSTANTIATION VAR-NUM.  Read value of variable VAR-NUM in INSTANTIATION according to INST-ACC.")
  (:method ((a inst-var-accessors) x i)
	   (funcall (aref (acc-readers a) i) x)))

(defgeneric set-var-val (a x i val)
  (:documentation "set-var-val INST-ACC INSTANTIATION VAR-NUM VAL.  Set value of variable number VAR-NUM in INSTANTIATION according to INST-ACC to be VAR-NUM.")
  (:method ((a inst-var-accessors) x i val)
	   (funcall (aref (acc-writers a) i) x val)))

(defun get-var-val-by-name (a x name)
  "get-var-val-by-name INST-ACC INSTANTIATION VAR-NAME.  Like get-var-val except lookup by name rather than number."
  (get-var-val a x (get-var-num a name)))

(defun set-var-val-by-name (a x name val)
  "set-var-val-by-name INST-ACC INSTANTIATION VAR-NAME VAL.  Like set-var-val except lookup by name rather than number."
  (set-var-val a x (get-var-num a name) val))

(defgeneric get-var-num (a name)
  (:method ((a inst-var-accessors) name)
	   (funcall (acc-var-num a) name)))

(defgeneric var-names (a)
  (:documentation "var-names INST-ACC.  Return set of variable names.")
  (:method ((a inst-var-accessors))
	   (acc-var-names a)))

(defgeneric num-vars (a)
  (:documentation "num-vars INST-ACC.  Return number of variables.")
  (:method (a)
	   (size (var-names a))))

(defgeneric detect-invalid-inst (a i)
  (:documentation "detect-invalid-inst INST-ACC INSTANTIATION.  Return nil if INSTANTIATION is a well-formed instantiation.  Otherwise, return an object denoting the reason why it is not.  Intended for error-handling, and so may not be fast.")
  (:method (a i)
	   (declare (ignore a i))
	   'reason-unknown)
  (:method ((a inst-var-accessors) i)
	   (funcall (acc-invalid-inst-detector a) i)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making various kinds of accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; association lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <alist-accessors> ()
  ((keys :type list :initarg :keys :reader keys)
   (test :type function :reader test :initarg :test :initform #'eql)))

(defun make-alist-accessors (keys &optional (test #'eql))
  "make-alist-accessors KEYS TEST.  
KEYS is a list of keys
TEST is an equality testing function that defaults to #'eql
Make instantiation accessors for storing data in an association list with keys KEYS, compared using TEST"
  (make-instance '<alist-accessors> :keys keys :test test))

(defmethod create-inst ((a <alist-accessors>))
  (mapcar #'(lambda (k) (cons k 'uninstantiated)) (keys a)))

(defmethod get-var-val ((a <alist-accessors>) inst i)
  (if (listp inst)
      (let ((k (nth i (keys a))))
	(cdr (check-not-null (assoc k inst :test (test a))
			     "Entry for ~a in alist ~a" k  inst)))
    'illegal-instantiation))

(defmethod set-var-val ((a <alist-accessors>) inst i v)
  (let ((k (nth i (keys a))))
    (let ((entry (assoc k inst :test (test a))))
      (aif entry
	   (setf (cdr it) v)
	   (progn
	     (assert (member k (keys a) :test (test a))
		 nil "Key ~a not permitted to be set for alist ~a accessed using ~a.  Legal set of keys is ~a." k inst a (keys a))
	     (assert inst)
	     (push (cons k v) inst))))))

(defmethod get-var-num ((a <alist-accessors>) name)
  (position name (keys a) :test (test a)))

(defmethod var-names ((a <alist-accessors>))
  (keys a))

(defmethod detect-invalid-inst ((a <alist-accessors>) i)
  (let ((keys (copy-list (keys a)))
	(test (test a)))
    (if (listp i)
	(dolist (entry i (awhen keys (list 'keys-not-present it)))
	  (unless (consp entry) 
	    (return-from detect-invalid-inst (list 'has-nonpair entry)))
	  (unless (member (car entry) keys :test test)
	    (return-from detect-invalid-inst (list 'has-illegal-key (car entry))))
	  (setf keys (delete (car entry) keys :test test)))
      'not-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  todo check indices in bounds

(defun make-vec-accessors (n)
  "make-vec-accessors NAMES.
Make instantiation accessors corresponding to storing instantiation in a vector.  NAMES is either a vector of variable names, or a fixnum, in which case the names are 0,1,...,N-1."
  (etypecase n
    (fixnum (make-array n :initial-contents (below n)))
    (vector n)))

(defmethod create-inst ((a array))
  (make-array (length a) :initial-element 'uninstantiated))

(defmethod get-var-val ((a array) x i)
  (aref x i))

(defmethod set-var-val ((a array) x i val)
  (setf (aref x i) val))

(defmethod get-var-num ((a array) name)
  (check-not-null (position name a :test #'equalp)
		  "Var ~a not found in ~a" name a))

(defmethod var-names ((a array))
  a)

(defmethod detect-invalid-inst ((a array) i)
  (if (vectorp i)
      (let ((l (length a))
	    (m (length i)))
	(unless (= l m)
	  (list 'wrong-length m)))
    'not-vector))
	
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; single object accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-single-object-accessors ()
  'single-object)

(defmethod create-inst ((a (eql 'single-object)))
  nil)

(defmethod get-var-val ((a (eql 'single-object)) x i)
  (assert (eq i 0))
  x)

(defmethod get-var-num ((a (eql 'single-object)) name)
  (declare (ignore name))
  0)

(defmethod var-names ((a (eql 'single-object)))
  '(()))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  todo check indices in bounds


(defun make-list-accessors (n)
  (etypecase n
    (fixnum (below n))
    (list n)))

(defmethod create-inst ((a list))
  (make-list (length a) :initial-element 'uninstantiated))

(defmethod get-var-val ((a list) x i)
  (if (listp x)
      (nth i x)
    'illegal-instantiation))

(defmethod set-var-val ((a list) x i val)
  (setf (nth i x) val))

(defmethod get-var-num ((a list) name)
  (check-not-null (position name a :test #'equalp)
		  "var ~a not found in ~a" name a))

(defmethod var-names ((a list))
  a)

(defmethod detect-invalid-inst ((l list) i)
  (loop
      with n = (length l)
      with k = i
      for j below n
		    
      if (null k)
      return (list 'incorrect-length j)
	       
      unless (consp k)
      return (list 'element-not-pair j k)
	       
      do (setf k (cdr k))
	   
      finally (unless (null k)
		(list 'incorrect-length (+ n (length k))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-struct-accessors (struct-name-dec field-descs)
  "Macro make-struct-accessors NAME FIELD-DESCS.

Create accessors (see package doc) for a structure of type NAME (not evaluated).  FIELD-DESCS (not evaluated) is a list of descriptions of variables or sets of variables.  Each description is either
- a symbol, which means the value is stored in the field of that name
- a list (FIELD-NAME ARRAY N) where FIELD-NAME (not evaluated) is the name of the field, ARRAY (not evaluated) is the symbol 'array, and N *evaluates* to a fixnum.  This refers to a set of N variables, stored in FIELD-NAME, which is an array of length at least N.

As an example, the code (let ((n 6))
  (make-struct-accessors foo (bar (baz array n) qux)))
evaluates to the accessors for a structure of type foo, where variable 1 is the field bar, variables 2-7 are stored in the field baz, which is an array, and variable 8 is in the field qux.  The names of the variables are 'bar, '(baz 0), ..., '(baz 5), 'qux."
  
  (let (actual-conc-name struct-name)
    (when (listp struct-name-dec)
      (setf struct-name (first struct-name-dec))
      (destructuring-bind (&key conc-name) (rest struct-name-dec)
	(setf actual-conc-name conc-name)))
    
    (orf actual-conc-name (intern-compound-symbol struct-name-dec "-"))
    (orf struct-name struct-name-dec)
		       
    (labels 
	((make-acc (field-name)
	   (intern-compound-symbol actual-conc-name field-name)))
  
      (with-gensyms (inst i h x name j val)
	`(make-inst-var-accessors
	  
	  :var-names ',(mapcar (lambda (d)
				 (if (symbolp d)
				     d
				   (first d)))
			       field-descs)
      
	  :creator
	  (lambda ()
	    (let ((,inst (,(intern-compound-symbol 'make- struct-name))))
	      ,@(loop
		    for f in field-descs
		    if (symbolp f)
		    collect `(setf (,(make-acc f) ,inst) 'uninstantiated)
		    else
		    collect `(setf (,(make-acc (first f)) ,inst)
			       (make-array ,(third f) :initial-element 'uninstantiated)))
	      ,inst))
      

	  :readers
	  (coerce
	   (append
	    ,@(loop 
		  for f in field-descs
		  if (symbolp f)
		  collect `(list #',(make-acc f))
		  else
		  collect 
		  `(mapset
		    'list
		    (lambda (,i)
		      #'(lambda (,x)
			  (aref (,(make-acc (first f)) ,x) ,i)))
		    ,(third f))))
	   'vector)
	
	  :writers
	  (coerce 
	   (append
	    ,@(loop
		  for f in field-descs
		  if (symbolp f) 
		  collect `(list #'(lambda (,x ,val) (setf (,(make-acc f) ,x) ,val)))
		  else 
		  collect
		  `(mapset
		    'list
		    (lambda (,i)
		      #'(lambda (,x ,val) (setf (aref (,(make-acc (first f)) ,x) ,i) ,val)))
		    ,(third f))))
	   'vector)
	
	  :var-num
	  (let ((,h (make-hash-table :test #'equal))
		(,i -1))
	    ,@(loop
		  for f in field-descs
		  if (symbolp f)
		  collect `(setf (gethash ',f ,h) (incf ,i))
		  else
		  collect
		  `(dotimes (,j ,(third f))
		     (setf (gethash (list ',(first f) ,j) ,h) (incf ,i))))
	    (lambda (,name)
	      (gethash ,name ,h))))))))
	  

