(defpackage qe-taxi-prog
  (:documentation "td-taxi-prog.lisp -  Alisp program used in the qe-taxi domain
Functions
---------
qe-taxi-prog
")

  (:use qe-taxi
	common-lisp
	utils
	alisp-prog)
  (:export qe-taxi-prog
	   N
	   S
	   E
	   W
	   P
	   D))

  


(in-package qe-taxi-prog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access functions for state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-env-accessor taxi-pos taxi-env-state-pos)

(defun pass-src (i)
  (aref (taxi-env-state-src (env-state)) i))

(defun pass-dest (i)
  (aref (taxi-env-state-dest (env-state)) i))

(def-env-accessor num-pass taxi-env-state-n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partial program for Taxi domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nav (loc)
  (until (equal (taxi-pos) loc)
    (with-choice nav-choice (dir '(N E S W))
		 (action nav-move dir))))

(defun get-pass (i)
  (call nav-src (nav (pass-src i)))
  (action pickup 'P))

(defun put-pass (i)
  (call nav-dest (nav (pass-dest i)))
  (action dropoff 'D))

(defun serve-next-pass ()
  (with-choice pass-choice (i (num-pass))
	       (call (get-pass i))
	       (call (put-pass i))))

(defun qe-taxi-prog ()
  (loop
    (call (serve-next-pass))))





