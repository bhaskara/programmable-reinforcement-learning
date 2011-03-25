(defpackage footman-prog
 (:documentation "footman-prog.lisp - Alisp program for footman-env.lisp domain
Functions
---------
footman-prog")

 (:use footman-env
       stratagus-env
       common-lisp
       utils
       alisp-prog)
 (:export footman-prog))

(in-package footman-prog)

(defsubroutine choose-to-attack (u enemy cmd-stack)
  (with-choice attack-choice (atk enemy) 
    (setf (gethash u cmd-stack) (format nil "4 ~a" atk))))

(defun footman-prog-not-work ()
    (loop
       (let ((my-living-units (units-belonging-to-of-type 0 0 (alisp:get-state)))
	    (enemy-living-units (units-belonging-to-of-type 1 0 (alisp:get-state)))
            (cmd-stack (make-hash-table)))
	      (loop for unit-i in my-living-units do
	        (call (choose-to-attack unit-i enemy-living-units cmd-stack)))
              (action joint-cmd cmd-stack)
       )))

(defun footman-prog ()
  (let ((my-living-units (units-belonging-to-of-type 0 0 (alisp:get-state)))
	(enemy-living-units (units-belonging-to-of-type 1 0 (alisp:get-state)))
        (cmd-stack (make-hash-table)))
	  (loop for unit-i in my-living-units do
	    (call (choose-to-attack unit-i enemy-living-units cmd-stack)))
	  (print (hash-to-alist cmd-stack))
          (action joint-cmd (hash-to-alist cmd-stack))
          (let ((trans-act (acons 1 "0" '())))
	   (loop (action continue-cmd trans-act)))))