(in-package mdp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hierarchical smdps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <hierarchical-smdp> (<smdp>)
  ()
  (:documentation "An abstract subclass of <smdp>.  Making a subclass of <smdp> be a subclass of <hierarchical-smdp> constitutes a commitment to implement a method for the generic function level for the class."))

(defgeneric level (m s)
  (:documentation "level HIER-SMDP OMEGA.  Return the 'level' of the state OMEGA in this smdp.  The level is a nonnegative integer where 0 is the 'top' level.  It is assumed that successive states in any trajectory of the SMDP have levels differing by at most 1."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabular hierarchical smdps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <hierarchical-tabular-smdp> (<hierarchical-smdp> <tabular-smdp>)
  ((level-vector :reader level-vec :writer set-level-vec :type (simple-array * 1)))
  (:documentation "Combines hierarchical and tabular smdps.  Level information is stored in a vector.  Used internally by hierarchical DP algorithms."))


(defmethod tabular-smdp ((smdp <hierarchical-smdp>))
  (let ((m (make-instance '<hierarchical-tabular-smdp> :smdp smdp)))
    (set-level-vec (mapset 'vector (lambda (s) (level smdp s)) (state-set smdp)) m)
    m))

(defmethod level ((smdp <hierarchical-tabular-smdp>) i)
  (aref (level-vec smdp) i))


