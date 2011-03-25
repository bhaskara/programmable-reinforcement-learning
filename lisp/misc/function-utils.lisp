;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function-utils.lisp
;; general utilities for functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building functions from other functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO correct in compose case

(defmacro fn (expr &optional (num-args 1) )
  "Macro FN EXPR &optional (NUM-ARGS 1).  Build up complex functions from expressions.  E.g. (fn (and integerp oddp)) expands to a function that returns true iff its argument is an odd integer.  Taken from Paul Graham's On Lisp.  See the description at the beginning of Chapter 15 for more details.  Have added the optional argument NUM-ARGS.  The idea is to be able to write something like (fn (* + -) 2) to return the equivalent of (lambda (x y) (* (+ x y) (- x y))).  It works in simple cases right now, but is unlikely to interact well with 'compose."
  `#',(rbuild expr num-args))

(defun rbuild (expr num-args)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
    (if (eq (car expr) 'compose)
	(build-compose (cdr expr))
      (build-call (car expr) (cdr expr) num-args))))

(defun build-call (op fns num-args)
  (let ((ga nil))
    (dotimes (i num-args)
      (push (gensym) ga))
    `(lambda ,ga
       (,op ,@(mapcar #'(lambda (f) `(,(rbuild f num-args) ,@ga))
		      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		   (if fns
		       `(,(rbuild (car fns) 1)
			 ,(rec (cdr fns)))
		     g)))
	  (rec fns)))))










