;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clone.lisp
;;
;; Defines operations for making fresh copies of various compound types.  This file
;; contains methods for clone for 
;; - numbers
;; - symbols
;; - strings
;; - sequences
;; - hash-tables
;; - arrays of any dimension
;;
;; Also defines clone-into
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)


(defgeneric clone (x)
  (:documentation "clone X.  Return a fresh copy of the object X.  Implementations of this method for a particular type should usually guarantee that the original X will be unaffected by any modifications to the returned object (though see comments for the hash-table case in clone.lisp).  Implementations for compound objects such as arrays recursively descend X, and so assume that clone methods are defined for each of the components of X.  Finally, note that the return value of (clone X) is not guaranteed to be #'equal to X. 

See also the def-struct-clone-method macro for structures.
"))


(defgeneric copy-into (x y)
  (:method-combination progn :most-specific-last)
  (:documentation "Generic function copy-into X Y.

Y must be an object that is a member of a subtype of the type of X. 

Makes it be the case that, in some sense depending on the particular type, Y becomes the same as X, and returns nothing.  This generic function uses progn method-combination, in most-specific-last order.  So, given classes foo, bar, and baz, where baz is a subclass of foo and bar, then, if X and Y are of class baz, the effective method first calls the copy-into method with signature (foo foo), then the one with signature (bar bar), and finally the one with signature (baz baz).  

This operation is typically not used in isolation, but to implement clone in a cleaner way in areas of the class hierarchy containing multiple inheritance."))






  
  
(defmethod clone ((x number))
  x)

(defmethod clone ((x string))
  x)

(defmethod clone ((x symbol))
  x)

(defmethod clone ((x cons))
  (cons (clone (car x)) (clone (cdr x))))

(defmethod clone ((x vector))
  (map 'vector #'clone x))


(defmethod clone ((x array))
  (loop
      with y = (make-array (array-dimensions x))
      for i below (array-total-size x)
		  
      do (setf (row-major-aref y i)
	   (clone (row-major-aref x i)))
	 
      finally (return y)))
	 

(defmethod clone ((x hash-table))
  "hash-table clone - reuse the keys but clone the values.  Thus, we are loosely interpreting 'modifications to the returned object' to exclude modifications to the keys in the hash-table"
  (loop
      with h = (make-hash-table :test (hash-table-test x) :size (hash-table-size x))
      for k being each hash-key in x using (hash-value v)
					    
      do (setf (gethash k h) (clone v))
	 
	 finally (return h)))
	       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-struct-clone-method (struct-type constructor conc-name cloned-fields copied-fields)
  "Macro def-struct-clone-method STRUCT-TYPE CONSTRUCTOR CONC-NAME CLONED-FIELDS COPIED-FIELDS
Define a clone method for a structure type.

None of the arguments are evaluated.
STRUCT-TYPE - symbol that names this structure type.
CONSTRUCTOR - symbol that is the name of constructor for this structure type
CONC-NAME - symbol that is used as a prefix for field references.  This is usually the name of the structure type followed by a '-'
CLONED-FIELDS - list of names of fields (as symbols) which are to be cloned
COPIED-FIELDS - list of names of fields that are just copied over.

The structure type must have already been defined when this macro is evaluated."
  (with-gensyms (x)
    `(defmethod clone ((,x ,struct-type))
       (,constructor
	,@(loop
	      for f in cloned-fields
	      collect (intern (symbol-name f) 'keyword)
	      collect `(clone (,(intern 
				 (concatenate 'string 
				   (symbol-name conc-name)
				   (symbol-name f)))
			       ,x)))
		     
	,@(loop
	      for f in copied-fields
	      collect (intern (symbol-name f) 'keyword)
	      collect `(,(intern (concatenate 'string 
				   (symbol-name conc-name)
				   (symbol-name f)))
			,x))))))
	    





(in-package cl-user)