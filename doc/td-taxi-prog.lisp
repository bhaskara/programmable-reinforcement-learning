(defpackage td-taxi-prog
  (:documentation "td-taxi-prog.lisp -  Alisp program for Dietterrich's taxi domain
Functions
---------
td-taxi-prog
")

  (:use td-taxi-env
	common-lisp
	utils
	alisp-prog)
  (:export td-taxi-prog
	   N
	   S
	   E
	   W
	   P
	   D))

  


(in-package td-taxi-prog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access functions for state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-env-feature passenger-loc pass-loc)
(def-env-feature passenger-src pass-source)
(def-env-feature passenger-dest pass-dest)
(def-env-feature pos taxi-pos)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partial program for Taxi domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defsubroutine nav (loc)
  (until (equal (pos) loc)
    (with-choice (nav-choice (dir '(N E S W)))
      (action nav-move dir))))

(defsubroutine get-pass ()
  (call nav ((passenger-src)))
  (action pickup 'P))

(defsubroutine put-pass ()
  (call nav ((passenger-dest)))
  (action dropoff 'D))


(defun td-taxi-prog ()
  (loop 
    (choose top 
	    (call get-pass ())
	    (call put-pass ()))))



