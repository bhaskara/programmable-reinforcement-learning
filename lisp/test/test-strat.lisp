(defpackage test-strat
  (:use
   common-lisp
   utils
   stratagus-env 
   footman-env
    ))

(in-package test-strat)

(setf test-env (make-instance '<footman-env>))

(defun test-moves (e)
  (send test-env "COMMAND 2 2 0 0")
  (send test-env "COMMAND 4 2 1 0")
  (loop for(test i from 1 to 10 do 
    (send test-env "TRANSITION"))))

(defun select-units (e type player)
  (let ((unit-list (car (send e "LISPGET STATE"))))
    (helper-select unit-list type player)))

(defun helper-select (unit-list type player)
  (let ((unit-num (car (first unit-list)))
        (unit-obj (cdr (first unit-list))))
    (if (NULL unit-list) '()
      (if (and (= player (unit-player-id unit-obj)) 
               (= type (unit-type unit-obj)))      
        (cons unit-num (helper-select (cdr unit-list) type player))
        (helper-select (cdr unit-list) type player)))))

(defun send-all-units (e unit-list command)
  (loop for unit-num in unit-list do
    (let ((fs (format nil "~a ~a ~a" "COMMAND" unit-num command)))
      (send e fs))))

(defun update-unit-list (ulist e)
  (let ((env-state (send e "LISPGET STATE")))
    (loop for i in ulist do
      (if (= (unit-status (cdr (nth (- i 1) (car env-state)))) 0)
        (setq ulist (remove i ulist))))
    ulist))

(defun do-it (e)
  (lure-enemy e)
  (let ((enemy-unit-list (select-units e 0 1)))
    (loop until (NULL enemy-unit-list) do
      (let ((fs (format nil "~a ~a" "4" (car enemy-unit-list))))
        (send-all-units e (select-units e 0 0) fs)
	(send e "TRANSITION")
        (setq enemy-unit-list (update-unit-list enemy-unit-list e))
        (print enemy-unit-list)   
     ))
    'done
))

(defun lure-enemy (e)
  (send e "TRANSITION")
  (send e "TRANSITION")
  (send-all-units e (select-units e 0 0) "2 0 0")
  (send e "TRANSITION"))
