(defpackage rbe-progs
  (:documentation "
Package rbe-progs - various partial programs for the resource-balance environment.  See experiment 1 in the thesis.

Programs
--------
rbe-flat
*rbe-maxq-prog*
*rbe-prog*
*rbe-coord-prog*
*rbe-uncoord-prog*

Choices
-------
maxq-gather-gold
maxq-gather-wood
gather-wood
gather-gold
get-resource
dropoff-resource
maxq-get-resource
maxq-dropoff-resource
nav
pickup
wait 
dropoff
N
S
E
W
R
")
  (:use cl
	utils
	calisp-features
	calisp-prog)
  (:import-from
   rbe
   N S E W R P D)
  (:export
   rbe-flat
   *rbe-maxq-prog*
   *rbe-prog*
   *rbe-coord-prog*
   *rbe-uncoord-prog*
   maxq-gather-gold
   maxq-gather-wood
   maxq-get-resource
   maxq-dropoff-resource
   gather-wood
   gather-gold
   get-resource
   dropoff-resource
   nav
   pickup
   wait 
   dropoff
   N
   S
   E
   W
   R   
   ))

(in-package rbe-progs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A. flat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rbe-flat ()
  (let ((joint-actions 
	 (make-instance '<prod-set>
	   :alist-keys '(0 1)
	   :sets '((N S E W R P D) (N S E W R P D)))))
    (loop
      (with-choice action-choice 
	  (a joint-actions)
	(action a)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B. maxq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rbe-maxq ()
  (dolist (i (my-effectors))
    (spawn i maxq-peasant-top new-peasant () i)))

(defun maxq-peasant-top ()
  (loop
    (choose task-choice
	    (call (maxq-gather-wood))
	    (call (maxq-gather-gold)))))

(defun maxq-gather-wood ()
  (until (am-carrying-wood)
    (choose gather-wood-choice
	    (maxq-get-resource 
	     (with-choice forest-choice 
		 (loc (forest-locs))
	       (get-resource loc)))
	    (maxq-dropoff-resource (dropoff-resource)))))

(defun maxq-gather-gold ()
  (until (am-carrying-gold)
    (choose gather-gold-choice
	    (maxq-get-resource
	     (with-choice mine-choice 
		 (loc (mine-locs))
	       (get-resource loc)))
	    (maxq-dropoff-resource (dropoff-resource)))))

(defun maxq-get-resource (loc)
  (while (empty-handed)
    (choose get-res-choice
	    (nav (nav loc))
	    (pickup (action pickup 'P))
	    (wait (action wait-get-resource 'R)))
    ))

(defun maxq-dropoff-resource ()
  (until (and (equal (my-position) (base-loc)) 
	      (not (am-carrying-gold))
	      (not (am-carrying-wood)))
    (choose dropoff-res-choice
	    (nav (nav (base-loc)))
	    (dropoff (action dropoff 'D)))))

(defun nav (loc)
  (until (equal (my-position) loc)
    (with-choice nav-choice 
	(direction '(N S E W R))
      (action move direction))))
  
(defparameter *maxq-choice-priority-list*
    '((new-peasant) (task-choice) (gather-wood-choice gather-gold-choice) (forest-choice mine-choice) (get-res-choice dropoff-res-choice) (nav-choice)))

(defparameter *rbe-maxq-prog* 
    (make-instance '<calisp-program>
      :root-fn #'rbe-maxq 
      :choosing-thread-fn *maxq-choice-priority-list*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C. ordered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rbe-ordered ()
  (dolist (i (my-effectors))
    (spawn i ordered-peasant-top new-peasant () i)))

(defun ordered-peasant-top ()
  (loop
    (choose task-choice
	    (call (gather-wood))
	    (call (gather-gold)))))

(defun gather-wood ()
  (call get-resource ((choose-arg (forest-locs))))
  (call dropoff-resource ()))

(defun gather-gold ()
  (call get-resource ((choose-arg (mine-locs))))
  (call dropoff-resource ()))

(defun get-resource (loc)
  (call nav (loc))
  (action pickup 'P)
  (until (ready-to-move)
    (action wait-get-resource 'R)))

(defun dropoff-resource ()
  (call nav ((base-loc)))
  (action dropoff 'D))


(defparameter *choice-priority-list*
    '((new-peasant) (task-choice) (get-resource dropoff-resource) (nav) (nav-choice)))

(defparameter *rbe-prog* 
    (make-instance '<calisp-program>
      :root-fn #'rbe-ordered
      :choosing-thread-fn *choice-priority-list*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D. Coordinated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rbe-coord ()
  (dolist (i (my-effectors))
    (spawn i coord-peasant-top new-peasant () i)))

(defun coord-peasant-top ()
  (loop
    (let ((g (num-gathering-gold))
	  (w (num-gathering-wood)))
      (cond
       ((> g w) (call gather-wood ()))
       ((> w g) (call gather-gold ()))
       (t (choose task-choice
		  (call (gather-gold))
		  (call (gather-wood))))))))

(defparameter *ordering*
    '((task-choice) (new-peasant) (gather-wood gather-gold)
      (get-resource dropoff-resource) (nav) (nav-choice)))

(defun highest-priority (omega)
  (funcall 
   (ordered-choosing-threads-function *ordering*)
   omega))
	   
(defun single-highest-priority-thread (omega)
  (let ((thread-labels (highest-priority omega)))
    (case (first thread-labels)
      ((task-choice new-peasant) (list (first thread-labels)))
      (otherwise thread-labels))))

(defparameter *rbe-coord-prog*
    (make-instance '<calisp-program>
      :root-fn #'rbe-coord
      :choosing-thread-fn #'single-highest-priority-thread))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E. Uncoordinated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rbe-uncoord ()
  (dolist (i (my-effectors))
    (spawn i uncoord-peasant-top new-peasant () i)))

(defun uncoord-peasant-top ()
  (loop  
      for num-gathering-gold = (num-gathering-gold)
      for num-gathering-wood = (num-gathering-wood)
      for max-gold = (rbe:max-gold)
      for max-wood = (rbe:max-wood)
      for expected-gold = (+ num-gathering-gold (rbe:gold))
      for expected-wood = (+ num-gathering-wood (rbe:wood))
			  
      do (cond ((>= expected-gold max-gold) (call gather-gold ()))
	       ((>= expected-wood max-wood) (call gather-wood ()))
	       ((>= num-gathering-gold num-gathering-wood) (call gather-gold ()))
	       ((>= num-gathering-wood num-gathering-gold) (call gather-wood ()))
	       (t (choose task-choice
			  (call (gather-wood))
			  (call (gather-gold)))))))
		       


(defparameter *rbe-uncoord-prog*
    (make-instance '<calisp-program>
      :root-fn #'rbe-uncoord
      :choosing-thread-fn #'single-highest-priority-thread))
		       
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-status ()
  (aref (rbe:status (env-state)) (first (my-effectors))))

(defun am-carrying-wood ()
  (eql (my-status) 'rbe:carrying-wood))

(defun am-carrying-gold ()
  (eql (my-status) 'rbe:carrying-gold))

(defun empty-handed ()
  (eql (my-status) 'rbe:empty-handed))

(defun my-position ()
  (aref (rbe:pos (env-state)) (first (my-effectors))))

(defun ready-to-move ()
  ;; this works because the status is a list iff it equals '(gathering I) for some I>=0
  (not (listp (my-status))))

(defun num-gathering (gather-type)
  (loop
      for ts being each hash-value in (js-thread-states (combined-state))
      count (some-frame-has-name ts gather-type)))

(defun num-gathering-gold ()
  (num-gathering 'gather-gold))

(defun num-gathering-wood ()
  (num-gathering 'gather-wood))


(def-env-accessor forest-locs rbe:forest-locs)
(def-env-accessor mine-locs rbe:mine-locs)
(def-env-accessor base-loc rbe:base-loc)