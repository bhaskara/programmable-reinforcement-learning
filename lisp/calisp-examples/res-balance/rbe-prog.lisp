(defpackage rbe-prog
  (:documentation "Concurrent Alisp program for resource balance environment
rbe-prog
make-rbe-prog
gather-wood
nav-choice
task-choice
gather-gold
new-peasant
get-resource
dropoff-resource
G
N
S
E
W
R
P
D
loc
rbe-reward-decomp
rbe-reward-decomp2
")
  #+short-nicknames (:nicknames rp)

  (:use common-lisp
	utils
	set
	calisp-prog)
  (:import-from rbe
		N
		S
		E
		W
		R
		P
		D
		gold
		wood)
		
		
  (:export rbe-prog
	   make-rbe-prog
	   gather-wood
	   nav-choice
	   task-choice
	   gather-gold
	   new-peasant
	   G
	   N
	   S
	   E
	   W
	   R
	   P
	   D
	   loc
	   get-resource
	   dropoff-resource
	   rbe-reward-decomp
	   rbe-reward-decomp2))

(in-package rbe-prog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access functions for state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-env-accessor forest-locs rbe:forest-locs)
(def-env-accessor mine-locs rbe:mine-locs)
(def-env-accessor base-loc rbe:base-loc)

(defun my-position ()
  (aref (rbe:pos (env-state)) (first (my-effectors))))

(defun ready-to-move ()
  (not (listp (aref (rbe:status (env-state)) (first (my-effectors)))))
  ;; this works because the status is a list iff it equals '(gathering I) for some I>=0
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partial program for Resource-balance domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav (loc)
  (until (equal (my-position) loc)
    (with-choice nav-choice (direction '(N S E W R))
		 (action move direction))))

(defun dropoff-resource ()
  (call nav ((base-loc)))
  (action dropoff 'D))

(defun get-resource (loc)
  (call nav (loc))
  (action pickup 'P)
  (until (ready-to-move)
    (action wait-get-resource 'R)))

(defun gather-gold ()
  (call get-resource ((choose-arg (mine-locs))))
  (call dropoff-resource ()))

(defun gather-wood ()
  (call get-resource ((choose-arg (forest-locs))))
  (call dropoff-resource ()))

(defun rbe-prog ()
  (dolist (i (my-effectors))
    (spawn i peasant-top new-peasant () i))
  (loop (action no-op nil)))

(defun peasant-top ()
  (loop 
    (choose task-choice
	    (call (gather-wood))
	    (call (gather-gold)))))


(defun make-rbe-prog ()
  (make-instance '<calisp-program>
    :root-fn #'rbe-prog :choosing-thread-fn (ordered-choosing-threads-function '((new-peasant) (task-choice) (get-resource dropoff-resource) (nav) (nav-choice)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reward decomposition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rbe-reward-decomp (omega s a r s2)
  "rbe-reward-decomp OMEGA S A R S2.

Reward decomposer for partial program in rbe-prog for resource balance domain.  Each peasant receives rewards for pickup/dropoff, and collisions are split among the colliding peasants.  Cost-of-living is received by top thread."
  
  (declare (ignore omega r a))
  
  (cons
   (cons 'root-thread (- (rbe:cost-of-living s)))
   (let ((positions (rbe:pos s2))
	 (statuses (rbe:status s2))
	 (drop-rew (rbe:dropoff-reward s))
	 (max-gold (rbe:max-gold s2))
	 (max-wood (rbe:max-wood s2)))
		   
     (let ((gold-rewards 
	   (let ((num-gold (count 'rbe:dropoff-gold statuses)))
	     (unless (zerop num-gold)
	       (/ (* drop-rew
		     (- (min max-gold (rbe:gold s2)) (min max-gold (rbe:gold s))))
		  num-gold))))
	  (wood-rewards 
	   (let ((num-wood (count 'rbe:dropoff-wood statuses)))
	     (unless (zerop num-wood)
	       (/ (* drop-rew
		     (- (min max-wood (rbe:wood s2)) (min max-wood (rbe:wood s))))
		  num-wood)))))
       (mapset 'list
	       #'(lambda (i)
		   (cons i
			 (let ((my-pos (aref positions i)))
			   (+
			    (* -.5 (rbe:collision-cost s)
			       (1- (count my-pos positions :test #'equal)))
			    (case (aref statuses i)
			      (rbe:dropoff-gold gold-rewards)
			      (rbe:dropoff-wood wood-rewards)
			      (otherwise 0))))))
	       (rbe:num-peas s))))))
     
		 
(defun rbe-reward-decomp2 (omega s a r s2)
  "rbe-reward-decomp2 OMEGA S A R S2.

Reward decomposer for partial program in rbe-prog for resource balance domain.  Each peasant receives rewards for pickup/dropoff, and collisions are split among the colliding peasants.  Cost-of-living is split among peasant threads as well."
  
  (declare (ignore omega r a))
  
  (let ((positions (rbe:pos s2))
	(statuses (rbe:status s2))
	(drop-rew (rbe:dropoff-reward s))
	(col (/ (- (rbe:cost-of-living s)) (rbe:num-peas s)))
	(max-gold (rbe:max-gold s2))
	(max-wood (rbe:max-wood s2)))
    (let ((gold-rewards 
	   (let ((num-gold (count 'rbe:dropoff-gold statuses)))
	     (unless (zerop num-gold)
	       (/ (* drop-rew
		     (- (min max-gold (rbe:gold s2)) (min max-gold (rbe:gold s))))
		  num-gold))))
	  (wood-rewards 
	   (let ((num-wood (count 'rbe:dropoff-wood statuses)))
	     (unless (zerop num-wood)
	       (/ (* drop-rew
		     (- (min max-wood (rbe:wood s2)) (min max-wood (rbe:wood s))))
		  num-wood)))))
      (mapset 'list
	      #'(lambda (i)
		  (cons i
			(let ((my-pos (aref positions i)))
			  (+
			   col
			   (* -.5 (rbe:collision-cost s)
			      (1- (count my-pos positions :test #'equal)))
			   (case (aref statuses i)
			     (rbe:dropoff-gold gold-rewards)
			     (rbe:dropoff-wood wood-rewards)
			     (otherwise 0))))))
	      (rbe:num-peas s)))))
  


