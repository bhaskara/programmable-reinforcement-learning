;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils.lisp
;; 
;; General-purpose utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package utils)


(defun get-lambda-list (fn)
  "get-lambda-list FUNCTION.  Return the lambda-list of the function.  There's no CL function that does this but most implementations provide a function.  Will signal an error if not using one of the supported implementations (allegro, clisp, cmu, corman, gcl, lispworks, lucid, sbcl)"
  #+allegro (excl:arglist fn)
  #+clisp (sys::arglist fn)
  #+cmu (values (let ((st (kernel:%function-arglist fn)))
                  (if (stringp st) (read-from-string st)
                      (eval:interpreted-function-arglist fn))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase fn (symbol (fdefinition fn)) (t fn)))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (si:compiled-function-name fn)))))
          (get fn 'si:debug))
  #+lispworks (lw:function-lambda-list fn)
  #+lucid (lcl:arglist fn)
  #+sbcl (values (let ((st (sb-kernel:%function-arglist fn)))
                  (if (stringp st) (read-from-string st)
                      #+ignore(eval:interpreted-function-arglist fn))))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'arglist fn))) 



(declaim (inline indicator))
(defun indicator (x)
  "indicator X.  Returns 1 if X is true and 0 otherwise"
  (if x 1 0))

(declaim (inline xor nxor))
(defun xor (x y)
  "xor X Y.  Treats X and Y as generalized booleans and returns their logical XOR."
  (if x (not y) y))

(defun nxor (x y)
  "nxor X Y.  Treats X and Y as generalized booleans and returns the negation of their logical XOR, so either they must both be true or neither."
  (if x y (not y)))



;; TODO these next two functions are obsolete
(defun below (hi)
  "below N.  Return the list (0 1 2 ... N-1)"
  (loop for x below hi collecting x))

(defun consec (low hi &optional (step 1))
  "consec LOW HI &optional (STEP 1).  Returns the list (low low+step ... low+k*step) where k is the last one that is <= HI"
  (loop for x from low to hi by step collecting x))


(defun number-sequence (s)
  "number-sequence S.  given a sequence s, return a vector of pairs (i . s(i)) (for debugging purposes)"
  (map 'vector #'cons (consec 0 (length s)) s))

(defun hash-table-has-key (h x)
  "hash-table-has-key HASHTABLE X.  Does the HASHTABLE contain (an item that satisfies the table's test together with) X?"
  (mvbind (y pres)
      (gethash x h)
    (declare (ignore y))
    pres))

(defun hash-keys (h)
  "hash-keys HASHTABLE.  Return list of keys in some order.  Usually used for debugging - use a hash table iterator if efficiency matters."
  (loop for k being each hash-key in h collecting k))

(defun eql-hash-table (h1 h2)
  "eql-hashtable H1 H2.  Return t iff two hashtables have the eql values associated to each key.  Note that a key that maps to the value nil is considered to be the same as a nonexistent key."
  (let* ((keylist nil))
    (maphash (lambda (k v) (declare (ignore v)) (setf keylist (cons k keylist))) h1)
    (maphash (lambda (k v) (declare (ignore v)) (setf keylist (cons k keylist))) h2)
    (loop
	for k in keylist

	do (if (not (eql (gethash k h1) (gethash k h2)))
	     (return nil))
	
	finally (return t))))



(defun alist-to-hash (l &key (test #'eql))
  "alist-to-hash L &key (TEST #'eql).  Convert an alist to a hashtable.  No cloning happens."
  (let ((h (make-hash-table :test test)))
    (dolist (x l h)
      (setf (gethash (car x) h) (cdr x)))))


(defun hash-to-alist (h)
  "hash-to-alist H.  Convert a hashtable to an alist.  No cloning done."
  (let ((l nil))
    (maphash 
     #'(lambda (k v) (push (cons k v) l)) 
     h)
    l))

(defun print-hash-table-readably (h str)
  "print-hash-table-readably HASHTABLE STREAM.  Print the hashtable to the stream readably, i.e., such that using read (with the standard readtable) on that stream will yield a hashtable that is the same as HASHTABLE, under the assumption that the print-object methods for the keys and values in HASHTABLE respect the *print-readably* variable.

The hashtable test is also preserved.  Other optimization features such as the rehash-threshold, etc. may not.  The printing is done by converting the table to an alist.

If STREAM is nil, a string containing the output is returned.  Otherwise, nil is returned and the output is performed as a side-effect."

  (with-standard-io-syntax
    (let
	#+allegro ((excl:*print-nickname* t))
	(format str "#.(utils:alist-to-hash '")
	(write (hash-to-alist h) :stream str)
	(format str " :test #'~a)"
		(ecase (hash-table-test h)
		  (eq "eq")
		  (eql "eql")
		  (equal "equal")
		  (equalp "equalp"))))))


(defun read-object-from-file (name)
  "read-object-from-file FILENAME.  Reads a single object from the file and returns it."
  (with-open-file (f name :direction :input)
    (read f)))

(defun hash-table-select (h test)
  "hash-table-select H TEST returns list of keys of items in H whose value satisfies TEST"
  (loop
      for k being each hash-key in h using (hash-value v)
      if (funcall test v)
      collect k))

(defun copy-hash-table (h &key (value-copy-fn nil))
  "copy-hash-table H &key (VALUE-COPY-FN nil).  Create a new hash table of same type as H.  The keys are copied over and the values have VALUE-COPY-FN applied to them first, if it is provided."
  (let ((h2 (make-hash-table :test (hash-table-test h) :size (hash-table-size h))))
    (maphash
     (if value-copy-fn
	 (lambda (k v)
	   (setf (gethash k h2) (funcall value-copy-fn v)))
       (lambda (k v) (setf (gethash k h2) v)))
     h)
    h2))   
  


(defun pprint-hash (h &optional (s t))
  "pprint-hash H &optional (STREAM t).  Print the elements of the hashtable to stream STREAM, which is interpreted as in pprint.  Obeys the *print-length* variable*"
  (pprint-logical-block (s (hash-keys h) 
			   :suffix 
			   (let ((not-shown 
				  (if *print-length*
				      (max 0 (- (hash-table-count h) *print-length*))
				    0)))
			     (when (> not-shown 0)
			       (format nil " and ~R other item~:*~P not shown here." not-shown))))
    (loop
      (pprint-exit-if-list-exhausted)
      (let ((k (pprint-pop)))
	(format s "~a : ~a" k (gethash k h))
	(pprint-newline :mandatory s))))
  (values))
	


(defun classify-bin (x l u num-bins &aux (y (- x l)) (m (- u l)))
  "classify-bin X L U NUM-BINS divides [L,U] into NUM-BINS bins and classifies X into one of them (or asserts if it doesn't fall within the bounds)"
  (assert (and (>= y 0) (<= y m)) () "value out of bounds in classify")
  (if (= y 0)
      0
    (1- (ceiling (* y (/ num-bins m))))))





(defvar *debug-level* 0)
(defun debug-print (l &rest args)
  "debug-print L &REST ARGS.  If *debug-level* exceeds L, apply format to ARGS"
  (when (> *debug-level* L) (apply #'format args)))



(defun wait-until (condition interval)
  "wait-until CONDITION INTERVAL.  Calls the function CONDITION with no arguments, and if it returns false, go to sleep for INTERVAL seconds and repeat this process until it returns a non-nil value, which is then returned by wait-until."
  (loop
      for ret-val = (funcall condition)
      until ret-val
	    
      do (sleep interval)
	 
      finally (return ret-val)))
  

(defun force-format (str &rest args)
  "force-format STREAM &rest ARGS.  Call format to STREAM using ARGS, then call force-output to STREAM.  Used on streams that buffer output, to ensure that the output appears immediately."
  (apply #'format str args)
  (force-output str))


(defparameter *default-prompt-msg* "~&Press enter to continue. ")
(defun prompt (&optional msg)
  "prompt &optional MSG.  Print the prompt message to *query-io*, then do a read-line on this stream and return the result.If message is not provided, *default-prompt-msg* is used"
  (format *query-io* (or msg *default-prompt-msg*))
  (read-line *query-io*))




(defun make-evenly-spaced-intervals (min max num-true)
  "make-evenly-spaced-intervals MIN MAX NUM-TRUE.  

Suppose there is a counter variable V that goes from MIN to MAX.  We want a predicate P which is true at a set of NUM-TRUE evenly spaced integers, such that the gap between each successive pair is as large as possible and, the last one is MAX.  Works exactly for NUM-TRUE >= 0 and <= MAX-MIN.  For NUM-TRUE > MAX-MIN, just saves MAX-MIN times.  Treats NUM-TRUE = nil as 0.

Useful, for example, when saving a history of a given length for an algorithm."
  
  (let ((span (- max min)))
    (cond
     ((> num-true (1+ span)) (constantly t))
     ((or (null num-true) (= num-true 0)) (constantly nil))
     ((= num-true 1) (lambda (i) (eql i max)))
     (t (let ((inc (floor span (1- num-true))))
	  (lambda (i) (eql 0 (mod (- max i) inc))))))))



(defun round-decimal (x d)
  "round-decimal X D.  Round X to D decimal places.  For convenience, if X is not a number, just return it."
  (if (numberp x)
      (let ((base (expt 10 d)))
	(float (/ (round (* base x)) base)))
    x))


(defun build-symbol-name (&rest args)
  "build-symbol-name S1 ... Sn.  Each Si is a string or symbol.  Concatenate them into a single long string (which can then be turned into a symbol using intern or find-symbol."
  (apply #'concatenate 'string (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) args)))

(defun intern-compound-symbol (&rest args)
  "intern-compound-symbol S1 ... Sn.  Interns the result of build-symbol-name applied to the S."
  (intern (apply #'build-symbol-name args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extended real arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline between between2 my<= my<))

(defun my<= (a b)
  "my<= A B.  <=, but also allows A and B to be 'infty or '-infty"
  (or (eq a '-infty)
      (eq b 'infty)
      (and (numberp a)
	   (numberp b)
	   (<= a b))))

(defun my< (a b)
  "my< A B.  < but also allows A and B to be 'infty or '-infty"
  (not (my<= b a)))

(defun my>= (a b)
  "my>= A B.  >= but also allows A and B to be 'infty or '-infty"
  (my<= b a))

(defun my> (a b)
  "my> A B. > but also allows 'infy and '-infty"
  (not (my<= a b)))

(defun mymax (a b)
  "mymax A B.  Like MAX, but allows args to be 'infty or '-infty"
  (case a
    (infty 'infty)
    (-infty b)
    (otherwise (case b
		 (infty 'infty)
		 (-infty a)
		 (otherwise (max a b))))))

(defun mymin (a b)
  "mymin A B.  Like MIN, but allows args to be 'infty or '-infty"
  (case a
    (infty b)
    (-infty '-infty)
    (otherwise (case b
		 (-infty '-infty)
		 (infty a)
		 (otherwise (min a b))))))
  
      
(defun between (a b c)
  "between A B C.  Check if a is >= b and <= c.  Note that any of the values may be 'infty or '-infty"
  (and (my<= b a) (my<= a c)))

(defun between2 (a b c)
  "between2 A B C.  Check if a is >= b and < c.  Note that any of the values may be 'infty or '-infty"
  (and (my<= b a) (my< a c)))

(declaim (inline to-boolean))

(defun to-boolean (x)
  "to-boolean X.  nil if X is nil and t otherwise."
  (when x t))



(declaim (inline bit-true bit-false))

(defun bit-true (x)
  "Does the bit X equal 1?"
  (ecase x
    (1 t)
    (0 nil)))

(defun bit-false (x)
  "Does the bit X equal 0?"
  (ecase x
    (1 nil)
    (0 t)))




(declaim (inline abs-diff))

(defun abs-diff (x y)
  "abs-diff X Y.  |X-Y|."
  (abs (- x y)))



(declaim (inline set-if-unbound))

(defun set-if-unbound (slot-name object value)
  "set-if-unbound SLOT-NAME OBJECT VALUE.  If the slot named SLOT-NAME of OBJECT is unbound, set its value to VALUE."
  (unless (slot-boundp object slot-name)
    (setf (slot-value object slot-name) value)))

(defun symbol< (s1 s2)
  (string< (symbol-name s1) (symbol-name s2)))


(declaim (inline fill-format fill-format-nl))

(defun fill-format (ch num str before after &rest args)
  "fill-format CHAR NUM STR BEFORE AFTER &rest ARGS"
  (let ((middle (map 'string (constantly ch) (below num))))
    (format str (concatenate 'string before middle after) args)))

(defun fill-format-nl (ch num &optional (str t))
  "fill-format-nl CHAR NUM &optional (STREAM t).  Expands to code that prints NUM copies of CHAR on a newline to STREAM."
  (fill-format ch num str "~&" ""))



(declaim (inline check-not-null))
(defun check-not-null (x msg &rest args)
  "check-not-null X MSG &rest ARGS.  Check that X is not null and return it."
  (let ((val x))
    (assert val () (concatenate 'string (format nil msg args) " was unexpectedly nil."))
    val))


(defmacro undefmethod (name &rest args)
  (let ((arg (first args)))
    (typecase arg
      (cons (udm name nil arg))
      (t (udm name (list arg) (second args))))))

(defun udm (name qual specs)
  (let ((classes (mapcar #'(lambda (s) `(find-class ',s)) specs)))
    `(remove-method (symbol-function ',name)
		    (find-method (symbol-function ',name)
				 ',qual
				 (list ,@classes)))))



(defun print-string-unquoted (str s)
  (map nil
    #'(lambda (c) (write-char c str))
    s))

(defmacro without-quotes (&rest body)
  "without-quotes &rest BODY.  Execute the BODY, with the current pprint dispatch table temporarily replaced by one that doesn't print quotes or package names when printing symbols or strings."
  `(let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
     (set-pprint-dispatch 'string #'print-string-unquoted)
     (set-pprint-dispatch 'symbol #'(lambda (str s) (print-string-unquoted str (symbol-name s))))
     ,@body))


(defun pprint-tabulated-pair (str x y width &optional (newline t))
  "pprint-tabulated-pair STREAM X Y WIDTH &optional (NEWLINE t).
Should be called with a pprint-logical-block.  Prints X followed by a colon, followed by Y at a tabstop WIDTH away from the start of X, all to STREAM.  If NEWLINE, then a mandatory newline is printed first."
  
  (when newline
    (pprint-newline :mandatory str))
  (without-quotes
   (format str "~W:~2,V:@T~W" x width y)))


(defvar *mixins* (make-hash-table :test #'equal))
(defmacro create-mixin ((&rest parents) &rest args)
  "macro create-mixin (&rest PARENTS) &rest ARGS.

PARENTS : a list of symbols (unevaluated)
ARGS : a list

First checks to see if a previous call to this macro has been done with the same parents.  If not, defines a new class with the given parent list.  Then creates an instance of the class with the given arguments."
  
  (with-gensyms (class new-class-name c present)
    `(let ((,class (mvbind (,c ,present)
		       (gethash ',parents *mixins*)
		     (if ,present
			 ,c
		       (setf (gethash ',parents *mixins*)
			 (defclass ,new-class-name (,@parents) ()))))))
       (make-instance ,class ,@args))))



(define-condition exact-class-error (error)
  ((instance :initarg :instance :reader instance)
   (class-list :initarg :class-list :reader class-list))
  (:report (lambda (c s) 
	     (format s "Class name of ~a was not in the allowed list ~a"
		     (instance c) (class-list c)))))

(defun check-exact-class (x name)
  "check-exact-class INSTANCE CLASS-NAMES
INSTANCE - an object.
CLASS-NAMES -  Either a list of symbols, or a symbol, which is treated as a single-element list.

If the name of (class-of INSTANCE) belongs to the given list of names, nothing happens.  Otherwise, an error is signalled, of type exact-class-error.

The macro is useful to protect against situations when the standard semantics of method combination doesn't quite do the right thing.  For example, suppose a generic function named foo is defined, and a method is defined for foo for instances of class bar.  Later, a class named baz is defined, but the programmer forgets to define a method for foo.  Code that calls foo on instances of baz will, rather than failing, silently use the method from bar.  Using check-exact-class in the method for foo for bar can be used to guard against this possibility."
  
  (let ((class-list 
	 (etypecase name
	   (symbol (list name))
	   (list name))))
    (unless (member (class-name (class-of x)) class-list)
      (error 'exact-class-error :instance x :class-list class-list))))

(in-package cl-user)


  




