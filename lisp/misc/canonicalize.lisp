;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc/canonicalize.lisp
;; Defines the generic function canonicalize
;;
;; TODO : it should be possible to avoid this most of the time by using
;; equalp hash-tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

(defgeneric canonicalize (x)
  (:documentation "canonicalize X.  Return a 'canonical' representative of the set of all objects that are 'the same' as X.  The intended meaning is that, whenever X and Y satisfy #'utils:same, then (canonicalize X) and (canonicalize Y) will be #'equal.  This is useful when making hashtables, etc. of complex objects.

Methods
1. For numbers, strings, and symbols, just returns x.
2. For a cons, separately canonicalize the car and cdr, then cons them back together
3. For a vector, turn it into a list first, then canonicalize")
  (:method ((x number)) x)
  (:method ((x symbol)) x)
  (:method ((x string)) x)
  (:method ((x cons)) (cons (canonicalize (car x)) (canonicalize (cdr x))))
  (:method ((x vector)) (map 'list (lambda (y) (canonicalize y)) x)))




(defmacro def-struct-canonicalize-method (struct-type conc-name can as-is)
  "macro def-struct-canonicalize-method STRUCT-TYPE CONC-NAME CANONICALIZED-FIELDS AS-IS-FIELDS."
  (let ((x (gensym)))
    `(defmethod canonicalize ((,x ,struct-type))
       (list
	,@(mapcar (lambda (f) `(canonicalize (,(intern-compound-symbol conc-name f) ,x))) can)
	,@(mapcar (lambda (f) `(,(intern-compound-symbol conc-name f) ,x)) as-is)))))


(defmacro gendefstruct (struct-options &rest descs)
  "Macro gendefstruct STRUCT-OPTIONS &rest DESCS.

Defines a structure type and clone and canonicalize methods for it.

STRUCT-OPTIONS (not evaluated).  Something suitable to be the first argument to defstruct, either an atom or a list of options.
DESCS (not evaluated).  List of descriptions.  Each is a list where the first element is the field description (as used in defstruct).  This is followed by two optional arguments, both unevaluated symbols.  The first is either clone, copy, or leave-out (the default being copy), and indicates how to deal with this field when cloning the structure.  The second is either canonicalize, as-is, or leave-out (default being canonicalize), and indicates how to deal with this field when canonicalizing the structure."
  
  (flet ((get-option (opt-name) 
	   (when (listp struct-options)
	     (second (find opt-name (rest struct-options) :key #'first)))))
    (let ((struct-type (if (listp struct-options) (first struct-options) struct-options)))
      (let ((cloned-fields nil)
	    (copied-fields nil)
	    (canonicalized-fields nil)
	    (as-is-fields nil)
	    (conc-name (or (get-option ':conc-name) (intern-compound-symbol struct-type "-")))
	    (const-name (intern-compound-symbol "MAKE-" struct-type)))
	(map nil
	  (lambda (desc)
	    (destructuring-bind (f-name &optional (clone-opt 'copy) (can-opt 'canonicalize))
		desc
	      (let ((name (if (symbolp f-name) f-name (first f-name))))
		(when (eq clone-opt 'clone)
		  (push name cloned-fields))
		(when (eq clone-opt 'copy)
		  (push name copied-fields))
		(when (eq can-opt 'canonicalize)
		  (push name canonicalized-fields))
		(when (eq can-opt 'as-is)
		  (push name as-is-fields)))))
	  descs)
	`(progn
	   (defstruct ,struct-options ,@(mapcar #'first descs))
	   (def-struct-clone-method ,struct-type ,const-name ,conc-name ,cloned-fields ,copied-fields)
	   (def-struct-canonicalize-method ,struct-type ,conc-name ,canonicalized-fields ,as-is-fields))))))


  
	   
	   