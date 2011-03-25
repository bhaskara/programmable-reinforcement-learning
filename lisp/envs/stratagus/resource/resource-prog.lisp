(defpackage resource-prog
 (:documentation "resource-prog.lisp - Alisp program for resource-env.lisp domain
Functions
---------
resource-prog")

 (:use resource-env
       stratagus-env
       common-lisp
       utils
       calisp-prog)
 (:export <resource-prog>))

(in-package resource-prog)

; CONSTANTS
(defconstant *map-length* 32)
(defconstant *map-width* 32)

(defclass <resource-prog> (<calisp-program>)
  ((choosing-thread-fn :initform #'single-thread-chooser)))

(defmethod start ((p <resource-prog>))
  (dolist (i (units-belonging-to-of-type 0 2 (env-state))) ; 0 -> red player (0); 2 -> peasant
    (spawn (cons 'peasant i) Root () i))
  (loop
   (action no-ops 0)))

(defun Root ()
  (loop
    (choose Root
      (Get-Gold (Get-Gold))
      (Get-Wood (Get-Wood))
      (Deposit  (Deposit)))))

(defun Get-Gold ()
  (until (equalp '(1) (unit-status-args (get-unit-state (car (my-effectors)) (env-state))))
    (with-choice harvest-choice (ct (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) ; choice - 0-15 regions, 16 harvest gold action
      (if (= ct 16)
	(progn
	  (action Harvest-Gold (format nil "6 1"))
	  (until (= 1 (unit-status (get-unit-state (car (my-effectors)) (env-state))))
            (action no-ops 0)))
	(Goto ct)))))

(defun Get-Wood ()
  (until (equalp '(2) (unit-status-args (get-unit-state (car (my-effectors)) (env-state))))
    (with-choice harvest-choice (ct (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) ; choice - 0-15 regions, 16 harvest wood action
      (if (= ct 16)	 
	(progn
	  (action Harvest-Wood (format nil "6 2"))
	  (until (= 1 (unit-status (get-unit-state (car (my-effectors)) (env-state))))
            (action no-ops 0)))
	(Goto ct)))))

(defun Goto (k)
  (action (cons goto k) (format nil "2 ~a ~a" (car (region-to-coordinate k)) (cdr (region-to-coordinate k))))
  (until (= 1 (unit-status (get-unit-state (car (my-effectors)) (env-state))))
    (action no-ops 0)))

(defun Deposit ()
    (until (null (unit-status-args (get-unit-state (car (my-effectors)) (env-state))))
    (with-choice dropoff-choice (ct (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) ; choice - 0-15 regions, 16 dropoff action
      (if (= ct 16)
	(progn
	  (action dropoff (format nil "7"))
	  (until (= 1 (unit-status (get-unit-state (car (my-effectors)) (env-state))))
            (action no-ops 0)))
	(Goto ct)))))

(defun region-to-coordinate (k)
  (if (and (>= k 0) (<= k 8))
    (cons (+ 4 (* (mod k 3) 9)) (+ 4 (* (floor (/ k 3)) 9)))
    (if (and (>= k 9) (<= k 11))
	(cons (+ 4 (* (mod k 3) 9)) 29)
        (if (and (>= k 12) (<= k 14))
	    (cons 29 (+ 4 (* (mod k 3) 9)))
	    (if (= k 15)
		(cons 29 29)
	        'error)))))

;; choosing-thread-fn that allows exactly one thread to choose at a time

(defun single-thread-chooser (omega)
  (pick-random (js-choosing-thread-ids omega)))

;; pick-random returns a random member of a list in a list

(defun pick-random (lst)
  (list (nth (random list-length) lst)))
    