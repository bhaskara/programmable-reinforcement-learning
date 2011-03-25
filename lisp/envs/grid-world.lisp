;; Package info
(defpackage grid-world
  (:documentation "envs/grid-world.lisp
Code for navigation in worlds whose map is a subset of a 2d grid.  The world is represented by a 2d array of integers where increasing the first coordinate corresponds to going south and increasing the second coordinate to going east. Locations in the grid are represented as two-element lists.  You can allow walls, doors, etc by using different values in the corresponding location in the array.  To use this when creating new environments, make a class that inherits from both <grid-world> and <env> (see resource-balance-env for an example).  To use a grid world directly, you can also just use a 2d boolean array (where t indicates a legal square) and all the operations will work.

Initargs
:world-map : map of the world
:legality-test : function that takes in the value at a location and returns t iff it is possible for the agent to be at this location (so it's not a wall, etc).  By default, is just the identity function.

Operations :
- set-test : change the legality test
- result : nominal result of moving in a given direction from a given state.  doesn't check legality.
- result-legal : nominal result of moving in a given direction, except if destination is not legal, just stay at source.
- rot-clockwise : clockwise rotation of a direction.
- rot-counterclockwise : counterclockwise rotation of a direction.
- is-legal-loc : is a location on the map and otherwise legal?
- noisy-move-dist : return the probability distribution corresponding to making a noisy move in a location
- sample-noisy-move : sample from above dist
- unif-grid-dist-sampler : a sampler that uniformly chooses among legal locations
- weighted-grid-dist-sampler : a sampler that chooses a location according to some distribution
- loc-value : value of a location. 
- dimensions : 2-element list representing dimensions of map
- manhattan-dist : manhattan distance between two locations
- shortest-path-dist : shortest-path distance

Exported symbols
- N, S, E, W : directions

Constants
- *moves* : list of legal directions")
  (:nicknames gw)
  (:use common-lisp
	utils)
  (:export <grid-world>
	   result
	   result-legal
	   rot-clockwise
	   rot-counterclockwise
	   N
	   S
	   E
	   W
	   R
	   noisy-move-dist
	   sample-noisy-move
	   set-test
	   world-map
	   manhattan-dist
	   *moves*
	   unif-grid-dist-sampler
	   dimensions
	   shortest-path-dist
	   is-valid-loc
	   is-legal-loc
	   loc-value
	   weighted-grid-dist-sampler))

(in-package grid-world)


;; TODO clean up


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <grid-world> ()
  ((wmap :initarg :world-map
	      :type array
	      :reader wmap)
   (legality-test :type function
		  :initarg :legality-test
		  :initform #'identity
		  :writer set-test
		  :reader test)
   (shortest-paths :reader shortest-paths
		   :writer set-shortest-paths
		   :initform nil)))
		   
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *moves* '(N E S W))
(intern 'R)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; unif-grid-dist-sampler
;;
;; gw : <grid-world>
;; 
;; returns a function that takes in zero arguments and returns a uniformly chosen
;; element from the set of locations in the grid whose value satisfies the test.
(defmethod unif-grid-dist-sampler ((gw <grid-world>))
  (let* ((grid (world-map gw))
	 (dims (array-dimensions grid))
	 (locs (loop
		   for i below (first dims)
		   append (loop
			      for j below (second dims)
			      if (funcall (test gw) (aref grid i j))
			      collect (list i j)))))
    (lambda () (elt locs (random (length locs))))))


;; weighted-grid-dist-sampler
;;
;; weights : array of same dimensions that contains probabilities
;;
;; returns a function that takes in zero arguments and returns a location (i j) sampled 
;; with probability proportional to (aref weights i j).  First samples the row, then the column
;; conditional on the row, to avoid quadratic cost per sample.
;; Doesn't check legality of locations - it doesn't get even passed in a <grid-world> so it doesn't
;; exactly belong here.
(defun weighted-grid-dist-sampler (weights)
  (let* ((dims (array-dimensions weights))
	 (r (first dims))
	 (c (second dims))
	 (unweighted-row-probs (make-array r))
	 (col-probs (make-array (list r c)))
	 (s (loop for i below r summing (setf (aref unweighted-row-probs i) (loop for j below c summing (aref weights i j))))))

    ;; compute P(col|row)  
    (loop
	for i below r
		    
	when (> (aref unweighted-row-probs i) 0)
	do (loop
	       for j below c
	       do (setf (aref col-probs i j) (/ (aref weights i j) (aref unweighted-row-probs i)))))
    
    (lambda ()
      
      ;; sample row
      (let* ((row (loop
		      with r1 = (random s)
		      for i below (1- r)
		      with s1 = (aref unweighted-row-probs 0)
			    
		      if (< r1 s1)
		      return i
			 
		      do (incf s1 (aref unweighted-row-probs (1+ i)))
		      finally (return (1- r))))
	     
	     ;; sample column conditional on row
	     (col (loop
		      with r2 = (random 1.0)
		      for j below (1- c)
		      with s2 = (aref col-probs row j)
				
		      if (< r2 s2)
		      return j
			     
		      do (incf s2 (aref col-probs row (1+ j)))
		      finally (return (1- c)))))
	(list row col)))))
	


(defun result (l a)
  "result LOC ACT.  returns a (fresh) list representing the nominal result of doing a in l.  doesn't check for legality of the move."
  (cond ((eq a 'N) (list (1- (first l)) (second l)))
	((eq a 'S) (list (1+ (first l)) (second l)))
	((eq a 'E) (list (first l) (1+ (second l))))
	((eq a 'W) (list (first l) (1- (second l))))
	(t (copy-list l))))




(defun result-legal (gw l a)
  "result-legal GW LOC ACTION.  If ACTION taken at LOC leads to a legal new location, return the new location, otherwise, just return a copy of LOC."
  (let ((d  (cond ((eq a 'N) (list (1- (first l)) (second l)))
		  ((eq a 'S) (list (1+ (first l)) (second l)))
		  ((eq a 'E) (list (first l) (1+ (second l))))
		  ((eq a 'W) (list (first l) (1- (second l))))
		  (t (copy-list l)))))
    (if (is-legal-loc gw d)
	d
      (copy-list l))))
    

(defun rot-clockwise (a)
  "rot-clockwise DIR.  DIR must be a member of gw:*moves*.  Return the 90-degree clockwise rotation of DIR."
  (ecase a (N 'E) (E 'S) (S 'W) (W 'N) (R 'R)))

(defun rot-counterclockwise (a)
  "rot-clockwise DIR.  DIR must be a member of gw:*moves*.  Return the 90-degree counterclockwise rotation of DIR."
  (ecase a (N 'W) (W 'S) (S 'E) (E 'N) (R 'R)))


(defgeneric dimensions (gw)
  (:documentation "dimensions GRID-WORLD.")
  (:method ((gw <grid-world>)) (array-dimensions (world-map gw)))
  (:method ((gw array)) (array-dimensions gw)))

(defun manhattan-dist (l1 l2)
  "manhattan-dist LOC1 LOC2."
  (loop
      for x in l1
      for y in l2
      summing (abs (- x y))))


(defun is-valid-loc (gw l)
  "is-valid-loc GW LOC.  Return t iff location is within dimensions of grid."
  (let ((y (first l))
	(x (second l))
	(dims (dimensions gw)))
    (and 
     (>= y 0)
     (< y (first dims))
     (>= x 0)
     (< x (second dims)))))
	   



(defun is-legal-loc (gw l)
  "is-legal-loc GW LOC.  Return t iff location is in grid, and legal (i.e. the agent can be at this location."
  (and (is-valid-loc gw l)
       (funcall (test gw) (loc-value gw l))))



;; noisy-move-dist
;;
;; gw : <grid-world>
;; l : location
;; a : action
;; msp : move success prob
;;
;;
;; RETURNS
;; list representing a prob dist : each element of the list is a pair (new-loc . prob)
;; 
;; Slip-prob is (1-msp)/2.  First sample applied action a-actual as a with prob msp,  and 
;; clockwise and counterclockwise rotations of a with prob slip-prob each.  Next, if 
;; result l a-actual is on the map and valid, that's the new loc.  otherwise the new loc is l.
;;
(defmethod noisy-move-dist ((gw <grid-world>) l a msp)
  "Return list of pairs of form (new-loc . prob) representing the transition distribution"
  (assert (member a *moves*) () "~a must be a valid move in grid-world:noisy-move-dist" a)
	  
    (let* ((slip-prob (/ (- 1 msp) 2))
	   (forward-loc (result l a))
	   (left-loc (result l (rot-counterclockwise a)))
	   (right-loc (result l (rot-clockwise a)))
	   (forward-prob (* msp (indicator (is-legal-loc gw forward-loc))))
	   (left-prob (* slip-prob (indicator (is-legal-loc gw left-loc))))
	   (right-prob (* slip-prob (indicator (is-legal-loc gw right-loc))))
	   (stay-prob (- 1 (+ forward-prob left-prob right-prob))))
      (loop
	  for p in (list forward-prob left-prob right-prob stay-prob)
	  for new-loc in (list forward-loc left-loc right-loc l)
			 
	  when (> p 0) collect (cons new-loc p))))



(defun compute-shortest-paths (gw)
  "compute-shortest-paths GRID-WORLD. Uses Floyd's algorithm.  Returns a hashtable that maps pairs of valid locations l1 and l2 to
the length of the shortest path between them, or nil if there's no such path.  Is cubic in the number of valid locations, so try not to call it too often."
  

  (let* ((wm (world-map gw))
	 (dims (array-dimensions wm))
	 (legal-locs (loop
			 for i below (first dims)
			 appending (loop
				       for j below (second dims)
				       if (funcall (test gw) (aref wm i j))
				       collect (list i j)))))
    

    (if (loop for i below (first dims)
	    always (loop for j below (second dims)
		       always (funcall (test gw) (aref wm i j))))
	
	;; a hack that avoids the expensive computation when the map has no walls    
	(loop
	    with distances = (make-hash-table :test #'equal)
	    finally (return distances)
		    
	    for l in legal-locs
	    do (loop
		   for l2 in legal-locs
		   do (setf (gethash (cons l l2) distances)
			(manhattan-dist l l2))))
			
    
    (loop
	with distances = (make-hash-table :test #'equal)
	finally (return distances)
	initially (loop
		      for l in legal-locs
		      do (setf (gethash (cons l l) distances) 0)
		      do (loop
			     for m in *moves*
			     with d of-type list
			     do (setf d (result l m))
			     when (is-legal-loc gw d)
			     do (setf (gethash (cons l d) distances) 1)))
		  
	for l1 in legal-locs
	do (loop
	       for l2 in legal-locs
	       do (loop
		      for l3 in legal-locs
		      do (let ((d1 (gethash (cons l2 l1) distances))
			       (d2 (gethash (cons l1 l3) distances))
			       (d3 (gethash (cons l2 l3) distances)))
			   (when (and d1 d2 (or (not d3) (> d3 (+ d1 d2))))
			     (setf (gethash (cons l2 l3) distances)
			       (+ d1 d2))))))))))
			       
			     

		  
			 
(defun shortest-path-dist (gw l1 l2)
   "shortest-path-dist GW L1 L2.  Returns the shortest-path distance between L1 and L2 in the grid.  If it hasn't been called before on this grid, then first runs Floyd's algorithm to compute and store all-pairs shortest paths.  So if the map is large, this could take a while."
   (let ((sp 
	  (or (shortest-paths gw)
	      (set-shortest-paths
	       (compute-shortest-paths gw)
	       gw))))
     (gethash (cons l1 l2) sp)))
	 

						       

;; loc-value
;;
;; gw : <grid-world>
;; l : location
;;
;; return value of this location.  Assumes location is valid.
(defmethod loc-value ((gw <grid-world>) l)
  (apply #'aref (world-map gw) l))


(defmethod world-map ((gw <grid-world>))
  (wmap gw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simpler version that just uses a 2d array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod world-map ((gw array))
  gw)

(defmethod test ((gw array))
  #'identity)

(defmethod shortest-paths ((gw array))
  (compute-shortest-paths gw))


(defmethod loc-value ((gw array) l)
  (apply #'aref gw l))


(in-package common-lisp-user)





