(defpackage td-taxi-env
  (:documentation " td-taxi-env.lisp
The taxi environment in Tom Dietterrich's MAXQ paper, in which there's
a single passenger and taxi, and the episode terminates once the
passenger has been dropped off, at which point a reward of 1 is given.
There's no discount.  This file supersedes td-taxi-mdp.lisp

Types
------
<td-taxi-env>

Examples
--------
make-example-env1
make-example-env2

Accessing parts of the state
----------------------------
pass-loc
taxi-pos
pass-source
pass-dest
env


Exported constants
------------------------------

Actions
'N
'S
'E
'W
'P (pickup)
'D (dropoff)
'F (refuel)

Passenger locations
-------------------
at-source
at-dest
in-taxi
")
  (:use common-lisp
	utils
	create-env
	grid-world)


  (:export <td-taxi-env>
	   make-example-env1
	   make-example-env2
	   N
	   S
	   E
	   W
	   P 
	   D
	   F
	   pass-loc
	   taxi-pos
	   pass-source
	   pass-dest
	   at-source
	   env
	   at-dest
	   in-taxi
	   ))

(in-package td-taxi-env)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (td-taxi-state (:conc-name ""))
  taxi-pos
  pass-loc
  pass-source
  pass-dest
  fuel
  env)



(defclass <td-taxi-env> (<fully-observable-env> <grid-world>)
  ((init-fuel :type fixnum
	      :initarg :if
	      :initform 1
	      :reader init-fuel)
   (fuel-decrease-prob :type float
		       :initarg :fp
		       :initform 0
		       :reader fuel-decrease-prob)
   (change-dest-prob :type float
		     :initarg :cd
		     :initform 0
		     :reader change-dest-prob)
   (move-success-prob :type float
		      :initarg :msp
		      :initform .9
		      :reader msp)
   (wall-collision-cost :type float
			:initarg :wcc
			:initform .5
			:reader wcc)
   (cost-of-living :type float
		   :initarg :col
		   :initform .1
		   :reader col)
   (pass-source :type list
		:initarg :ps
		:reader ps)
   (pass-dest :type list
	      :initarg :pd
	      :reader pd))
  (:documentation "
Constructor for <td-taxi-env> takes the following required initargs
:world-map - world map
:ps - passenger source list.  Passenger source sampled uniformly from these
:pd - passenger dest list.  Passenger dest sampled uniformly from these
:if - initial fuel.  1 by default.
:fp - probability of using up 1 unit of fuel on a given move.  0 by default.
:cd - probability of passenger changing destination en route

and the following optional initargs
:wcc - wall collision cost.  .5 by default.
:msp - move success prob.  .9 by default.
:col - cost of living.  .1 by default."))




   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *avail-actions* '(N E S W D P F))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((e <td-taxi-env>) &rest args &key world-map ps pd)
  (declare (ignore args))
  (assert (and world-map ps pd) () "worldmap, passenger source and dest, must be supplied when creating <td-taxi-env>")
  (set-test #'not-wall e))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations from <env>
;;
;; TODO : many of these are reimplementing stuff that's
;; in gridworld
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defun reward (e s a d)
  (if (is-terminal-state e s)
      0
  (- (indicator (eq (pass-loc d) 'at-dest))
     (col e)
     (* (wcc e)
	(indicator 
	 (and (equal (taxi-pos s) (taxi-pos d))
	      (member a *moves*)))))))


(defmethod sample-next ((e <td-taxi-env>) s a &aux (src (taxi-pos s)))
  (assert (member a *avail-actions*)  (a) "Action ~a not member of ~a" a *avail-actions*)
  (let ((next-s
  (make-td-taxi-state
   :taxi-pos
   (if (and (member a *moves*) (> (fuel s) 0))
       (let ((slip-prob (/ (- 1 (msp e)) 2))
	     (forward-loc (result src a))
	     (left-loc (result src (rot-counterclockwise a)))
	     (right-loc (result src (rot-clockwise a))))
	 (let ((forward-prob (* (msp e) (indicator (is-legal-loc e forward-loc))))
	       (right-prob (* slip-prob (indicator (is-legal-loc e right-loc))))
	       (left-prob (* slip-prob (indicator (is-legal-loc e left-loc)))))
	   (let ((stay-prob (- 1 (+ forward-prob left-prob right-prob))))
	     (prob:sample-multinomial
	      (list forward-loc right-loc left-loc src)
	      forward-prob right-prob left-prob stay-prob))))
     src)
   :pass-loc
   (if (eql a 'P)
       (if (equal src (pass-source s)) 
	   'in-taxi
	 (pass-loc s))
     (if (and (equal src (pass-dest s)) (eq (pass-loc s) 'in-taxi) (eql a 'D))
	 'at-dest
       (pass-loc s)))
   :pass-source (pass-source s) 
   :pass-dest   (let ((cd (change-dest-prob e)))
		  (if (eql (prob:sample-multinomial 2 cd (- 1 cd)) 0)
		      (prob:sample-uniformly (pd e))
		    (pass-dest s)))
   :fuel (if (eql a 'F)
	     (init-fuel e)
	   (let ((fp (fuel-decrease-prob e))
		 (f (fuel s)))
	     (prob:sample-multinomial (list (max 0 (1- f)) f) fp (- 1 fp))))
   :env e)))
    (values next-s (reward e s a next-s))))


(defmethod sample-init ((e <td-taxi-env>))
  (make-td-taxi-state
   :taxi-pos (funcall (unif-grid-dist-sampler e))
   :pass-loc 'at-source
   :pass-source	(prob:sample-uniformly (ps e))
   :pass-dest   (prob:sample-uniformly (pd e))
   :fuel	(init-fuel e)
   :env e))


(defmethod is-terminal-state ((e <td-taxi-env>) s)
  (eq (pass-loc s) 'at-dest))

(defmethod io-interface :before ((e <td-taxi-env>))
  (format t "~&Welcome to the td-taxi environment.  This environment models a taxi that moves around in a map, and must pick up a passenger and drop her off at the destination.  X's on the map represent walls and blank spaces are roads.  The taxi is represented by a t or T (capitalized once you have picked up the passenger).  The actions are 0,1,2,3 to move N,E,S,W, 4 to putdown the passenger, and 5 to pickup the passenger.~%~%"))
  

(defmethod avail-actions ((e <td-taxi-env>) s)
  (declare (ignore s))
  *avail-actions*)
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other methods on states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod print-object ((s td-taxi-state) str)
  (loop
      with e = (env s)
      with d = (dimensions e)
      for i from -1 to (first d)
      do (format str "~&")
      do (loop
	     for j from -1 to (second d)
	     do (cond ((or (= i -1) (= i (first d)) (= j -1) (= j (second d)))
		       (format str "X"))
		      ((eq (loc-value e (list i j)) 'wall) 
		       (format str "X"))
		      ((equal (taxi-pos s) (list i j))
		       (if (eq (pass-loc s) 'in-taxi)
			   (format str "T")
			 (format str "t")))
		      (t (format str " "))))
      finally (format str "~&Source : ~a  Dest : ~a  Fuel : ~a" (pass-source s) (pass-dest s) (fuel s))))
	 

(defmethod clone ((s td-taxi-state))
  (make-td-taxi-state
   :pass-source (clone (pass-source s))
   :pass-dest (clone (pass-dest s))
   :env (env s)
   :pass-loc (pass-loc s)
   :taxi-pos (clone (taxi-pos s))
   :fuel (fuel s)))

(defmethod same ((s td-taxi-state) (s2 td-taxi-state))
  (and
   (eq (env s) (env s2))
   (eql (pass-loc s) (pass-loc s2))
   (equal (taxi-pos s) (taxi-pos s2))
   (equal (pass-source s) (pass-source s2))
   (eql (fuel s) (fuel s2))
   (equal (pass-dest s) (pass-dest s2))))

(defmethod canonicalize ((s td-taxi-state))
  "convert to list"
  (list 'pos (taxi-pos s) 'pass (pass-loc s) 'source (pass-source s) 'dest (pass-dest s) 'fuel (fuel s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
		       
  
(defun not-wall (x)
  (not (eq 'wall x)))



(in-package common-lisp-user)














