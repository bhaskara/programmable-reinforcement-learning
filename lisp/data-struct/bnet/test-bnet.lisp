(defpackage test-bnet
  (:use bnet
	cl
	prob
	grid-world
	utils
	mdp-env
	set
	inst-vars
	help))

(in-package test-bnet)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 
(defun dist1 (x)
  (list (cons (* (first x) 8) 1)))

(defun dist2 (x)
  (list (cons (first x) .8) (cons (- 30 (first x)) .2)))

(defun dist3 (x) (declare (ignore x)) #(.1 .2 .7))

(defun dist4 (x)
  (list (cons (reduce #'+ x) 1)))

(setf desc1 
  (make-2tbn-var-desc
   :id 'A :domain '(0 8 16) :init-slice-parents '(B) :init-dist #'dist1 
   :prev-slice-parents '(A) :curr-slice-parents nil :trans-dist #'dist2))

(setf desc2
  (make-2tbn-var-desc
   :id 'B :domain '(0 1 2 8 9 10 16 17 18) :init-slice-parents () :init-dist #'dist3
   :prev-slice-parents '(B) :curr-slice-parents '(A) :trans-dist #'dist4))

(defstruct foo
  bar
  baz)

(defstruct qux
  oof)

(setf 2tbn (make-instance '<2tbn> :state-descs (list desc1 desc2) :state-acc (make-vec-accessors 2)))
(setf stbn (make-instance '<2tbn> :state-descs (list desc1 desc2) :state-acc (make-struct-accessors foo (bar baz))))
(setf sa2tbn (make-instance '<2tbn> :state-descs (list desc1 desc2) 
			    :state-acc (make-struct-accessors qux ((oof array 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taxi example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct taxi-es locs)
(def-struct-clone-method taxi-es make-taxi-es taxi-es- (locs) ())



(defun make-taxi-env (num-taxis map-size &key (collision-cost 10)
					      (cost-of-living 1)
					      (slip-prob .1))
  (make-2tbn-mdp-env
   (make-instance '<2tbn>
    :state-descs (make-taxi-state-descs num-taxis map-size  slip-prob)
    :action-descs (make-taxi-action-descs num-taxis)
    :reward-descs (make-reward-descs num-taxis collision-cost cost-of-living)
    :state-acc (make-struct-accessors taxi-es ((locs array num-taxis)))
    :action-acc (make-list-accessors num-taxis)
    :reward-acc (make-vec-accessors (1+ (/ (* num-taxis (1- num-taxis)) 2))))))

(defun make-taxi-action-descs (n)
  (mapset 'list
	  (lambda (i)
	    (make-2tbn-var-desc :id `(action ,i) :domain '(N E S W R)))
	  n))

(defun make-reward-descs (n coll-cost col)
  (cons (make-2tbn-var-desc :id 'cost-of-living :domain 'real-numbers
			    :prev-slice-parents () :curr-slice-parents ()
			    :reward-fn (constantly (- col)))
	(mapset 'list
		(lambda (x)
		  (let ((i (first x))
			(j (second x)))
		    (make-2tbn-var-desc :id `(coll-cost ,i ,j) :domain 'real-numbers
					:prev-slice-parents () :curr-slice-parents (list i j)
					:reward-fn (lambda (x) 
						     (if (equal (first x) (second x))
							 (- coll-cost)
						       0)))))
		(loop 
		    for i below n
		    append (loop for j below n
			       when (< i j)
			       collect (list i j))))))

(defun make-taxi-state-descs (num-taxis map-size  slip-prob)
		      
  (let ((slip-dist `((nil . ,(- 1 slip-prob)) (left . ,(/ slip-prob 2))
					      (right . ,(/ slip-prob 2))))
	(gw (make-instance '<grid-world> :world-map (make-array (list map-size map-size)
								:initial-element 0)))
	(locs (make-instance 'prod-set:<var-set> :sets (list map-size map-size)
			     :element-type 'list)))
  
    (labels ((taxi-trans-dist (par)
	       (destructuring-bind (old-loc act) par
		 (if (eq act 'R)
		     (make-deterministic-dist old-loc)
		   (make-rv-dist (lambda (x)
				   (result-legal gw old-loc
						 (cond
						  ((eq x 'left) (rot-counterclockwise act))
						  ((eq x 'right) (rot-clockwise act))
						  (t act))))
				 slip-dist)))))
      (mapset 'list
	      (lambda (i)
		(make-2tbn-var-desc
		 :id i :domain locs :init-slice-parents nil 
		 :init-dist (lambda (x) (declare (ignore x)) (make-unif-dist-over-set locs))
		 :prev-slice-parents `(,i (action ,i)) :curr-slice-parents nil 
		 :trans-dist #'taxi-trans-dist))
	      num-taxis))))
		     
  
					      
			   
