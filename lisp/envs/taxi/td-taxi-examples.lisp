;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; envs/taxi/td-taxi-examples.lisp - some functions to create some specific
;; examples of <td-taxi-env>s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package td-taxi-env)

(defun make-example-env1 ()
  "Create an example td-taxi-env with a 2x3 world with a wall at (0,1), in which the source is always (1,0) and the destination is either (0,2) or (1,1).  Moves are deterministic and costs are default (see <td-taxi-env>)"
  (let ((pass-source '((1 0)))
	(pass-dest '((0 2) (1 1)))
	(wm
	 (let ((a (make-array '(2 3) :initial-element 'road)))
	   (setf (aref a 0 1) 'wall)
	   a)))
    (make-instance '<td-taxi-env> :world-map wm :ps pass-source :pd pass-dest :msp 1)))

(defun make-example-env2 ()
  "Create an example td-taxi-env with a 6x6 world in which the source is always (0,0) and the destination is either (0,5) or (5,0).  Move-success-prob is .95 and costs are default (see <td-taxi-env>)"
  (let ((pass-source '((0 0)))
	(pass-dest '((0 5) (5 0)))
	(wm (make-array '(6 6) :initial-element 'road)))
    (make-instance '<td-taxi-env> :world-map wm :ps pass-source :pd pass-dest :msp .95)))
