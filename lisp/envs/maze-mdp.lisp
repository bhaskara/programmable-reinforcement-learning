;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maze-mdp.lisp
;; A 'maze-world' mdp involving navigating in a rectangular grid
;;
;; Extends <mdp>.  New operations :
;; - make-maze-mdp : constructor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage maze-mdp
  (:use common-lisp
	utils
	prod-set
	grid-world
	mdp)
  (:export make-maze-mdp
	   res))

(in-package maze-mdp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <maze-mdp> (<mdp> <grid-world>)
  (
   (rewards :type array
		:initarg :rewards
		:reader rewards)
   (cost-of-living :type integer
		   :initarg :col
		   :reader col)
   (action-set :reader action-set
	       :initarg :action-set)
   (move-success-prob :type float
		      :initarg :msp
		      :reader msp)
   (term :type array
	 :initarg :term
	 :reader term)
   (collision-cost :type float
		   :initarg :col-cost
		   :reader col-cost)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-maze-mdp
;; constructor for <maze-mdp>
;;
;; world-map : a 2-d array containing a map of the world.  a position with t is an open
;; square and one with nil is a wall.
;;
;; KEYs
;; rewards : array that represents reward gained for entering each square.  by default all 0.
;; cost-of-living : cost per step of existence.  1 by default.
;; move-success-prob : a number representing the probability of a move succeeding.  0.9
;; by default
;; termination : an array that says which squares are terminal.  by default always false.
;; collision-cost : cost of hitting a wall or the edge.  1 by default.
;; allow-rests : whether to allow staying still.  t by default.
;;
;;
;; Available actions are 0 for North, 1 for east, 2 for south, 3 for west, 4 for rest
;; A move succeeds with move-success-prob.  If not, then 
;; choose uniformly from one of the perpendicular directions and
;; attempt to move in that direction.  If an attempted move runs into a wall or goes off the edge, 
;; location doesn't change and we pay collision-cost.  
;; the origin is at the top-left-corner, S increases the first coordinate and E increases the second coordinate.
(defun make-maze-mdp (world-map &key (dims (array-dimensions world-map)) (rewards (make-array dims :initial-element 0))
				     (move-success-prob .9) (termination (make-array dims :initial-element nil))
				     (collision-cost 1) (cost-of-living 1) (allow-rests t))

  (make-instance '<maze-mdp> :world-map world-map :legality-test #'identity
		 :rewards rewards :msp move-success-prob :term termination
		 :col-cost collision-cost :col cost-of-living
		 :state-set (make-instance '<var-set> :element-type 'list :sets (list (consec 0 (1- (first dims))) (consec 0 (1- (second dims)))))
		 :action-set (if allow-rests '(N E S W R)
			       '(N E S W))))



(defmethod reward ((m <maze-mdp>) s a d)
  (if (terminal? m s)
      0
    (if (possible-result m s a d)
	(-
	 (if (eq a 'R)
	     0
	   (if (equal s d)
		 
					; we must have hit a wall
	       (- (col-cost m))

	     (apply #'aref (rewards m) d)))
	   
	 (col m))
      0)))

  


(defmethod trans-dist ((m <maze-mdp>) s a)
  (if (or (eq a 'R) (terminal? m s))
      (list (cons s 1.0))
    (let* ((move-success-prob (msp m))
	   (slip-prob (/ (- 1 move-success-prob) 2))
	   (forward-prob (if (is-valid-move m s a) move-success-prob 0))
	   (left-prob (if (is-valid-move m s (rot-counterclockwise a)) slip-prob 0))
	   (right-prob (if (is-valid-move m s (rot-clockwise a)) slip-prob 0)))
      (list (cons (result s a) forward-prob)
	    (cons (result s (rot-clockwise a)) right-prob)
	    (cons (result s (rot-counterclockwise a)) left-prob)
	    (cons s (- 1 (+ forward-prob right-prob left-prob)))))))


(defmethod terminal? ((m <maze-mdp>) s)
  (apply #'aref (term m) s))
  


;; res
;; l : location (2-element list)
;; a : action (integer)
;;
;; returns the destination of the action if it's legal, or the original location if not
(defmethod res ((m <maze-mdp>) l act)
  (let ((a (first act)))
    (if (member a *moves*)
	(let ((d (result l a)))
	  (if (is-legal-loc m d)
	      d
	    l))
      l)))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions for moving around in grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; is a valid in s in this map?
(defun is-valid-move (m s a)
  (is-legal-loc m (result s a)))



;; Helper function that tests if a move could have happened
(defun possible-result (m s a d)
  (let* ((d1 (result s a))
	 (d2 (result s (rot-clockwise a)))
	 (d3 (result s (rot-counterclockwise a))))
    (and (is-legal-loc m s)
	 (is-legal-loc m d)

	 (or (equal d d1)
	     (equal d d2)
	     (equal d d3)
	     (and (equal d s)
		  (not (and (is-legal-loc m d1)
			    (is-legal-loc m d2)
			    (is-legal-loc m d3))))))))



(in-package common-lisp-user)














