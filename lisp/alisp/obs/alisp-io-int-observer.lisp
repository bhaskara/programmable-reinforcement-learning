;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/obs/alisp-io-int-observer.lisp - defines <alisp-io-int-observer>, the 
;; kind of observer used by the io interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *hist* nil
  "When the IO interface is running, holds the trajectory of joint states seen during execution.  This provides a quick way to generate joint state objects - just use the IO interface to get where you want, then stop and use the last item *hist*.  Set to a fresh vector on each run (so the old *hist* can just be copied to some other place and won't get overwritten when a new run happens).")


(defclass <alisp-io-int-observer> (<alisp-observer>)
  ((output-stream :initarg :str
		  :reader str
		  :initform t
		  :type stream)
   (choice-label-stack :initform nil
		       :accessor choice-label-stack))
  (:documentation "Class <alisp-io-int-observer> is used by the ALisp io-interface, and just prints out what happens in the ALisp program to the stream."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-execution ((obs <alisp-io-int-observer>))
  (format (str obs) "~&Starting execution.")
  (setf *hist* (make-array 10 :adjustable t :fill-pointer 0)))

(defmethod inform-finish-execution ((obs <alisp-io-int-observer>))
  (format (str obs) "~&~%Execution complete."))

(defmethod inform-start-episode ((obs <alisp-io-int-observer>) s)
  (setf (choice-label-stack obs) nil)
  (format (str obs) "~&~%Beginning new episode in state ~&~a" s))

(defmethod inform-env-step ((obs <alisp-io-int-observer>) a r s2 term)
  (format (str obs) "~&~%Action ~a was done in the environment, yielding reward ~a.  New state is ~&~a. " a r s2)
  (when term (format (str obs) "The episode has terminated.")))

(defmethod inform-part-prog-terminated ((obs <alisp-io-int-observer>) omega)
  (unless (y-or-n-p "~&~%The partial program has terminated at state ~a.~&Start a new episode?" omega)
    (error 'rlm-last-step-reached)))

(defmethod inform-alisp-step ((obs <alisp-io-int-observer>) omega u)
  (if (exit-state? omega)
      (format (str obs) "~&~%At exit state with label ~a" (second (js-pc omega)))
    (format (str obs) "~&~%At state with label ~a, chose ~a" (second (js-pc omega)) u)))

(defmethod inform-end-choice-block ((obs <alisp-io-int-observer>) omega)
  (declare (ignore omega))
  (format (str obs) "~&~%Leaving choice block."))
  



  