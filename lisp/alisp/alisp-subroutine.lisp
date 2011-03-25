;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/alisp-subroutine.lisp
;; code relating to alisp subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)


(defun lookup-alisp-subroutine (fn-name)
  "lookup-alisp-subroutine FN-NAME.  Returns list of param names for FN-NAME."
  (multiple-value-bind (val pres)
      (gethash fn-name *subroutine-description-table*)
    (assert pres nil "Subroutine ~a not found in table" fn-name)
    val))
  

(defmacro defsubroutine (fn-name lambda-list &rest args)
  "defsubroutine FN-NAME LAMBDA-LIST &rest ARGS.  Equivalent to defun, except also adds entry for FN-NAME to *subroutine-description-table*."
  (setf (gethash fn-name *subroutine-description-table*) lambda-list)
  `(defun ,fn-name ,lambda-list ,@args))