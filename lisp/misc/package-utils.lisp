(defpackage package-utils
  (:use common-lisp)
  (:export export-from
	   symbols
	   external-symbols))

(in-package package-utils)


(defun symbols (&optional (p *package*))
  "symbols PACKAGE (default current package).  Return list of all symbols of PACKAGE, a package designator."
  (let ((s nil))
    (do-symbols (v p)
      (push v s))
    s))

(defun external-symbols (&optional (p *package*))
  "external-symbols PACKAGE (default current package).  Return list of external symbols of PACKAGE, a package designator."
  (let ((s nil))
    (do-external-symbols (v p)
      (push v s))
    s))


(defmacro export-from (package &rest symbol-names)
  "export-from PACKAGE &rest SYMBOL-NAMES.  PACKAGE is a designator for a package.  Each element of SYMBOL-NAMES is a string naming a symbol that is present in PACKAGE.  Calling export-from causes the given symbols to be made available as external symbols of the current package, so any package that uses the current package will have access to them."
  (let ((symb (gensym)))
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (dolist (symb-name ',symbol-names)
	 (let ((,symb (intern (string-upcase symb-name) ,package)))
	   (import ,symb)
	   (export ,symb ,package) ;; this is just so the help feature sees this symbol
	   (export ,symb))))))