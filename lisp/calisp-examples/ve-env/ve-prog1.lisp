
(defpackage ve-prog
  (:documentation "Package ve-prog.

Types
-----
<ve-prog>

Program symbols
---------------
footman
peasant
wait

")
  (:export
   <ve-prog>
   footman
   peasant
   wait)
      
  (:use cl
	set
	utils
	calisp-prog)
  (:import-from 
   ve-env
   train-footman
   train-peas
   footman
   peasant
   wait
   gather
   attack 
   defend)
  )

(in-package ve-prog)

(defclass <ve-prog> (<calisp-program>)
  ())


(defmethod start ((p <ve-prog>))
  (spawn 'train train () 0)
  (spawn 'peasant peasants () 1)
  (spawn 'footman footmen () 2)
  )

(defmethod assign-effectors ((p <ve-prog>) omega effectors)
  (let ((units (ve-env:units (calisp-features:js-env-state omega))))
    (mapcar (lambda (e)
	      (aref units (1- e)))
	    effectors)))


(defun train ()
  (loop
    (choose train-choice
	    (footman (wait-and-train 'train-footman (footman-cost)))
	    (peasant (wait-and-train 'train-peas (peas-cost)))
	    (wait (action wait 'wait)))))


(defun wait-and-train (cmd cost)
  (until (>= (resources) cost)
    (action wait 'wait))
  (action train cmd))


(defun peasants ()
  (loop (action gather 'gather)))

(defun footmen ()
  (loop
    (with-choice 
	attack-choice (attackers (powerset (my-effectors)))
	(awhen attackers
	       (spawn (get-new-thread-id 'attack)
		      attack ()
		      it)))
    (action defend 'defend)))


(defun attack ()
  (let ((alive-enemies
	 (loop
	     for i from 0
	     for h across (enemy-hps)
	     when (> h 0)
	     collect i)))
    (with-choice 
	enemy-choice (target alive-enemies)
	(while (and (> (aref (enemy-hps) target) 0)
		    (> (length (my-effectors)) 1))
	  (action attack (list 'attack target)))))
  (reassign finish-attack (my-effectors) 'footman :wait-action 'defend))

    
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-env-accessor resources ve-env:resources)
(def-env-accessor peas-cost ve-env:peas-cost)
(def-env-accessor footman-cost ve-env:footman-cost)
(def-env-accessor enemy-hps ve-env:enemy-hps)


