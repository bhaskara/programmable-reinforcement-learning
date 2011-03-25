(defpackage qe-taxi
  (:documentation "Package for taxi environments.

Creating taxi environments
--------------------------
make-taxi-2tbn
make-qe-taxi-smdp
taxi-init-dist

Actions
-------
N,S,E,W,P, D

Accessing the state
-------------------
taxi-env-state-pos
taxi-env-state-pass-change
taxi-env-state-src
taxi-env-state-dest
taxi-env-state-generosity
taxi-env-state-pass-in-taxi
taxi-env-state-term
taxi-env-state-traffic
taxi-env-state-n
taxi-term


Symbols used in state rep
---------------------------------
dropoff
unchanged
empty
term
nonterm
")

  (:use bnet
	cl
	prob
	grid-world
	utils
	set
	prod-set
	inst-vars)
  (:export 
   ;; creation
   make-taxi-2tbn
   make-qe-taxi-smdp
   taxi-init-dist

   ;; accessing state
   taxi-env-state-pos
   taxi-env-state-pass-change
   taxi-env-state-src
   taxi-env-state-dest
   taxi-env-state-generosity
   taxi-env-state-pass-in-taxi
   taxi-env-state-term
   taxi-env-state-traffic
   taxi-env-state-n
   taxi-term
   
   ;; actions
   N S W E P D
   
   ;; symbols in state rep
   dropoff
   unchanged
   empty
   term
   nonterm
   ))
   
   

(in-package qe-taxi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state rep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct taxi-env-state
  pos
  pass-change
  src
  dest
  generosity
  pass-in-taxi
  term
  n
  world-map
  traffic)

(def-struct-clone-method taxi-env-state make-taxi-env-state taxi-env-state-
			 () (pos pass-change src dest generosity pass-in-taxi term traffic n world-map))

(defmethod canonicalize ((s taxi-env-state))
  (list (taxi-env-state-pos s) (taxi-env-state-pass-change s) (taxi-env-state-src s)
	(taxi-env-state-dest s) (taxi-env-state-generosity s) (taxi-env-state-pass-in-taxi s)
	(taxi-env-state-term s) (taxi-env-state-traffic s)))

(defmethod print-object ((s taxi-env-state) str)
  (loop
      with wm = (taxi-env-state-world-map s)
      with num-rows = (array-dimension wm 0)
      with num-cols = (array-dimension wm 1)
      with box-size = 3
      with (row row-offset col col-offset pos)
		
      for y below (* box-size num-rows)
      do (multiple-value-setq (row row-offset)
	   (floor y box-size))
	 (format str "~&")
	 (loop
	     for x below (* box-size num-cols)
	     do (multiple-value-setq (col col-offset)
		  (floor x box-size))
		(setf pos (list row col))
		(format str
			(or
			 (cond
			  ((or (= 0 col-offset) (= 0 row-offset)) "X")
			  ((and (= 1 col-offset) (= 1 row-offset))
			   (when (equal (taxi-env-state-pos s) pos)
			     (format nil "~a" 
				     (let ((i (taxi-env-state-pass-in-taxi s)))
				       (if (integerp i) i "t")))))
			  ((and (= 1 col-offset) (= 2 row-offset))
			   (let ((ind (position pos (taxi-env-state-src s) :test #'equal)))
			     (when ind (format nil "~a" ind))))
			  ((and (= 2 col-offset) (= 2 row-offset))
			   (let ((ind (position pos (taxi-env-state-dest s) :test #'equal)))
			     (when ind (format nil "~a" ind)))))
			 " "))
	     finally (format str "X~&"))
      finally (loop repeat (1+ (* box-size num-cols)) do (format str "X"))
	      (format str "~&Generosities : ~a~&Pass-in-taxi : ~a Change : ~a Term : ~a" 
		      (taxi-env-state-generosity s) (taxi-env-state-pass-in-taxi s) 
		      (taxi-env-state-pass-change s) (taxi-env-state-term s))
	      ))
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taxi pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun make-pos-cpd (gw slip-prob)
  (make-cpd 
   (prev-pos act)
   (let ((slip-dist `((nil . ,(- 1 slip-prob)) (left . ,(/ slip-prob 2))
					       (right . ,(/ slip-prob 2)))))
     (if (member act '(P D))
	 (make-deterministic-dist prev-pos)
       (make-rv-dist (lambda (x)
		       (result-legal gw prev-pos
				     (cond
				      ((eq x 'left) (rot-counterclockwise act))
				      ((eq x 'right) (rot-clockwise act))
				      (t act))))
		     slip-dist)))))


(defun make-pos-desc (wm slip-prob init-positions)
  (make-2tbn-var-desc
   :id 'pos :domain (make-instance '<var-set> :sets (array-dimensions wm) :element-type 'list)
   :init-slice-parents () :init-dist (constantly (make-unif-dist-over-set init-positions))
   :prev-slice-parents '(pos action) :curr-slice-parents ()
   :trans-dist (make-pos-cpd wm slip-prob)))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pass state change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pass-change-cpd (par)
  (make-deterministic-dist
   (or
    (destructuring-bind (pos act src dest pass-in-taxi) par
      (if (eq act 'P)
	  (let ((ind (position pos src :test #'equal)))
	    (when (and ind (not (integerp pass-in-taxi))) ind))
	(when (and (eq act 'D)
		   (integerp pass-in-taxi)
		   (equal pos (aref dest pass-in-taxi)))
	  'dropoff)))
    'unchanged)))

(defun make-change-desc (n)
  (make-2tbn-var-desc
   :id 'pass-change :domain `(unchanged dropoff ,@(below n))
   :init-slice-parents nil :init-dist (constantly (make-deterministic-dist 'unchanged))
   :prev-slice-parents '(pos action src dest pass-in-taxi)
   :curr-slice-parents () :trans-dist #'pass-change-cpd))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-sources-dist (sources n)
  (make-instance '<sampling-without-replacement>
    :dist (make-unif-dist-over-set sources)
    :n n))

(defun make-src-cpd (sources n)
  (make-cpd
   (prev-src change)
   (if (eq change 'dropoff)
       (new-sources-dist sources n)
     (make-deterministic-dist prev-src))))

(defun make-src-desc (sources n)
  (make-2tbn-var-desc
   :id 'src :domain (make-instance '<var-set> :sets (loop repeat n collect sources))
   :init-slice-parents nil :init-dist (constantly (new-sources-dist sources n))
   :prev-slice-parents '(src) :curr-slice-parents '(pass-change)
   :trans-dist (make-src-cpd sources n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-dests-dist (dests n)
  (make-instance '<sampling-with-replacement>
    :dist (make-unif-dist-over-set dests)
    :n n))

(defun make-dest-cpd (dests n)
  (make-cpd
   (prev-dest change)
   (if (eq change 'dropoff)
       (new-dests-dist dests n)
     (make-deterministic-dist prev-dest))))

(defun make-dest-desc (dests n)
  (make-2tbn-var-desc
   :id 'dest :domain (make-instance '<var-set> :sets (loop repeat n collect dests))
   :init-slice-parents () :init-dist (constantly (new-dests-dist dests n))
   :prev-slice-parents '(dest) :curr-slice-parents '(pass-change)
   :trans-dist (make-dest-cpd dests n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generosity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun new-gens-dist (gen-dist n)
  (make-instance '<sampling-with-replacement> :dist gen-dist :n n))

(defun make-gen-cpd (gen-dist n)
  (make-cpd
   (prev-gen change)
   (if (eq change 'dropoff)
       (new-gens-dist gen-dist n)
     (make-deterministic-dist prev-gen))))
	      

(defun make-generosity-desc (gen-dist n)
  (make-2tbn-var-desc
   :id 'generosity :domain (make-instance '<var-set> :sets (loop repeat n collect (sample-space gen-dist)))

   :init-slice-parents () :init-dist (constantly (new-gens-dist gen-dist n))
   :prev-slice-parents '(generosity) :curr-slice-parents '(pass-change)
   :trans-dist (make-gen-cpd gen-dist n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; passenger in taxi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pass-in-taxi-cpd (par)
  (destructuring-bind (prev-pass-in-taxi change) par
    (make-deterministic-dist
     (if (eq change 'unchanged)
	 prev-pass-in-taxi
       (if (eq change 'dropoff)
	   'empty
	 (progn
	   (check-type change integer "a legal value for pass-in-taxi")
	   change))))))


(defun make-pass-in-taxi-desc (n)
  (make-2tbn-var-desc
   :id 'pass-in-taxi :domain (cons 'empty (below n))
   :init-slice-parents () :init-dist (constantly (make-deterministic-dist 'empty))
   :prev-slice-parents '(pass-in-taxi) :curr-slice-parents '(pass-change)
   :trans-dist #'pass-in-taxi-cpd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; termination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-term-cpd (term-prob)
  (make-cpd
   (change)
   (if (eq change 'dropoff)
       `((term . ,term-prob) (nonterm . ,(- 1 term-prob)))

     (make-deterministic-dist 'nonterm))))


(defun make-term-desc (term-prob)
  (make-2tbn-var-desc
   :id 'term :domain '(term nonterm) :init-slice-parents ()
   :init-dist (constantly (make-deterministic-dist 'nonterm))
   :prev-slice-parents nil :curr-slice-parents '(pass-change)
   :trans-dist (make-term-cpd term-prob)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; traffic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-traffic-cpd (traffic-dist)
  (make-cpd
   (prev-traffic change)
   (if (eq change 'dropoff)
       (cond-dist traffic-dist prev-traffic)
     (make-deterministic-dist prev-traffic))))
  
(defun make-traffic-desc (init trans dom)
  (make-2tbn-var-desc
   :id 'traffic :domain dom :init-slice-parents () :init-dist (constantly init)
   :prev-slice-parents '(traffic) :curr-slice-parents '(pass-change)
   :trans-dist (make-traffic-cpd trans)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dropoff rewards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dropoff-reward ()
  (make-cpd
   (pass-in-taxi gen change)
   (if (eq change 'dropoff)
       (aref gen pass-in-taxi)
     0)))


(defun make-dropoff-reward-desc ()
  (make-2tbn-var-desc
   :id 'dropoff :domain 'natural-numbers
   :prev-slice-parents '(pass-in-taxi generosity) 
   :curr-slice-parents '(pass-change)
   :reward-fn (make-dropoff-reward)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-toll-reward (toll-fn)
  (make-cpd
   (change traffic)
   (if (eq change 'dropoff)
       (- (funcall toll-fn traffic))
     0)))

(defun make-toll-reward-desc (toll-fn)
  (make-2tbn-var-desc
   :id 'toll :domain 'natural-numbers 
   :prev-slice-parents () :curr-slice-parents '(pass-change traffic)
   :reward-fn (make-toll-reward toll-fn)))


	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;; the 2tbn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-taxi-2tbn (wm n
		       &key
		       (traffic-trans-dist (constantly (make-deterministic-dist 'no-traffic)))
		       (traffic-init-dist (make-deterministic-dist 'no-traffic))
		       (traffic-set '(no-traffic))
		       (slip-prob .1)
		       (cost-of-living 1)
		       (toll-fn (constantly 0))
		       (term-prob .2)
		       (gen-dist (make-deterministic-dist 5))
		       (valid-locs (loop for i below (array-dimension wm 0) 
				       append (loop for j below (array-dimension wm 1)
						  when (aref wm i j) collect (list i j))))
		       (sources valid-locs)
		       (dests valid-locs)
		       (taxi-start-pos valid-locs))
  "make-taxi-2tbn WORLD-MAP N &key TRAFFIC-TRANS-DIST TRAFFIC-INIT-DIST TRAFFIC-SET SLIP-PROB COST-OF-LIVING TOLL-FN TERM-PROB GEN-DIST SOURCES DESTS TAXI-START-POS

Create a 2tbn for the taxi example.  Params
WORLD-MAP : a 2d boolean array in which position i,j is legal if the array value is true
N : number of passengers
TRAFFIC-SET : set of possible traffic values.  by default is '(no-traffic)
TRAFFIC-INIT-DIST : initial distribution over traffic.  Default is always '(no-traffic)
TRAFFIC-TRANS-DIST : conditional dist of traffic evolution.  Default is always '(no-traffic)
SLIP-PROB : the probability that an intended navigation move 'slips' resulting in a perpendicular attempted move. .1 by default
COST-OF-LIVING : cost charged on each step.  1 by default.
TOLL-FN : toll charged as a function of traffic, per passenger.  Always 0 by default.
TERM-PROB : prob of termination after each dropoff.  .2 by default.
SOURCES : set of places where passengers can start.  by default is all legal locations.
DESTS : set of places where passengers want to go.  by default is all legal locations.
GEN-DIST : distribution over generosity of a new passenger (must be an integer).  By default is always 5.
TAXI-START-POS : set of places where taxi can start.  by default is all legal locations.

Return a '<2tbn>."
	 
  
  (make-instance '<2tbn>
    
    :state-descs 
    (list
     (make-pos-desc wm slip-prob taxi-start-pos)
     (make-change-desc n)
     (make-src-desc sources n)
     (make-dest-desc dests n)
     (make-generosity-desc gen-dist n)
     (make-pass-in-taxi-desc n)
     (make-term-desc term-prob)
     (make-traffic-desc traffic-init-dist traffic-trans-dist traffic-set)
     (make-param-desc 'world-map wm)
     (make-param-desc 'n n))
    
    :action-descs
    (list (make-2tbn-var-desc :id 'action :domain '(N E S W P D)))
    
    :reward-descs
    (list (make-2tbn-var-desc :id 'cost-of-living :domain 'real-numbers
			      :prev-slice-parents () :curr-slice-parents ()
			      :reward-fn (constantly (- cost-of-living)))
	  (make-dropoff-reward-desc)
	  (make-toll-reward-desc toll-fn))
    
    :state-acc (make-struct-accessors taxi-env-state (pos pass-change src dest generosity
							  pass-in-taxi term traffic
							  world-map n))
    :action-acc (make-single-object-accessors)
    
    :reward-acc (make-vec-accessors 3)))


(defun taxi-term (s)
  (eq (taxi-env-state-term s) 'term))
  