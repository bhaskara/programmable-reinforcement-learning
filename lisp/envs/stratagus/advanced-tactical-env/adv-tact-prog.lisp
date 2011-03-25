(defpackage adv-tact-prog
 (:documentation "adv-tact-prog.lisp - Alisp program for adv-tact-env.lisp domain
Functions
---------
adv-tact-prog")

 (:use adv-tact-env
       stratagus-env
       common-lisp
       utils
       calisp-prog)
 (:export <adv-tact-prog>))

(in-package adv-tact-prog)

(defclass <adv-tact-prog> (<calisp-program>)
  ((actions-list   :accessor tact-actions-list
	           :initform nil
                   :initarg act-list)))

(defmethod start ((p <adv-tact-prog>))
  (print "START!")
  (setf (tact-actions-list p) '(START))
  (let ((c (make-hash-table)))
    (dolist (u (units-belonging-to-except 0 (env-state) 59))
      (with-choice assignment-choice (tid (list 1 2 3 4))
        (print (format nil "Assigning new unit ~a to attack-thread ~a" u tid))
        (if (null (gethash tid c))
          (setf (gethash tid c) (list u))
          (setf (gethash tid c) (append (gethash tid c) (list u))))))
    (dolist (i (list 1 2 3 4))
      (if (not (null (gethash i c)))
        (spawn i combat-thread () (gethash i c)))))
  ; do no-ops with all the trivial effectors
  (loop 
    (setf (tact-actions-list p) (append (tact-actions-list p) (acons (my-effectors) 0 '())))
    (action null-ops 0)))

(defun combat-thread ()
  (loop
    (choose attack-choice
      (attack-target (attack))
      (defend-base (let ()
        (setf (tact-actions-list p) (append (tact-actions-list p) (acons (my-effectors) 0 '())))
        (action defend 0))  ))))

(defun attack ()
  (with-choice target-choice (u (units-belonging-to-except 1 (env-state) 59))
    (loop until (NULL (member u (units-belonging-to-except 1 (env-state) 59))) do
    (setf (tact-actions-list p) (append (tact-actions-list p) (acons (my-effectors) (format nil "4 ~a" u) '())))
    (action attacking-target (format nil "4 ~a" u)))))

(defun units-belonging-to-except (id s ty)
  "units-belonging-to-except PLAYER-ID STRAT-ENV-STATE TYPE.  Return list of ids of units belonging to this player except of the type specified by TYPE.  Only returns those units with HP > 0, to deal with the ghost units that stick around for a while after they die."
  (hash-table-select (strat-es-units s)
		     (lambda (us)
		       (and (< (unit-type us) 105) (not (= (unit-type us) ty)) (= (unit-player-id us) id) (> (unit-hp us) 0)))))