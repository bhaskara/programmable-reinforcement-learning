;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calisp/obs/calisp-io-int-observer.lisp - defines <calisp-io-int-observer>, 
;; the kind of observer used by the io interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package calisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *hist* nil
  "When the IO interface is running, holds the trajectory of joint states seen during execution.  This provides a quick way to generate joint state objects - just use the IO interface to get where you want, then stop and use the last item of *hist*.  Set to a fresh vector on each run (so the old *hist* can just be copied to some other place and won't get overwritten when a new run happens).")
;; TODO - this doesn't actually happen right now.  Also, it might be good to move this to a separate observer.


(defclass <calisp-io-int-observer> (<calisp-observer>)
  ((output-stream :initarg :str
		  :reader str
		  :initform *standard-output*
		  :type stream)
   (print-states :initarg :print-states :initform nil :reader print-states)
   (choice-label-stack :initform nil
		       :accessor choice-label-stack))
  (:documentation "Class <calisp-io-int-observer> is used by the concurrent ALisp io-interface, and just prints out what happens when running the program to the stream.

Initargs
:str - output stream
:print-states - whether to print out the states at each step.  Nil by default."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-execution ((obs <calisp-io-int-observer>))
  (format (str obs) "~&Starting execution.")
  (setf *hist* (make-array 10 :adjustable t :fill-pointer 0)))

(defmethod inform-finish-execution ((obs <calisp-io-int-observer>))
  (format (str obs) "~&~%Execution complete."))

(defmethod inform-start-episode ((obs <calisp-io-int-observer>) s)
  (setf (choice-label-stack obs) nil)
  (format (str obs) "~&~%Beginning new episode in state ~&~a." s))

(defmethod inform-env-step ((obs <calisp-io-int-observer>) a r s2 term)
  (format (str obs) "~&~%Action ~a was done in the environment, yielding reward ~a.  New state is ~&~a. " a r s2)
  (when term (format (str obs) "The episode has terminated.")))

(defmethod inform-part-prog-terminated ((obs <calisp-io-int-observer>))
  (unless (y-or-n-p "~&~%The partial program has terminated.  Start a new episode? ")
    (error 'crlm-last-step-reached)))

(defmethod inform-calisp-step ((obs <calisp-io-int-observer>) omega u)
  (let ((str (str obs)))
    (format str "~&~%")
    (when (print-states obs)
      (format str "At ")
      (pprint-calisp-state str omega))
      
    (format str "Made choice ~a" u)))

(defmethod inform-spawn-thread ((obs <calisp-io-int-observer>) id current-thread effectors)
  (declare (ignore effectors))
  (format (str obs) "~&Thread ~a spawning a thread with ID ~a."
	  current-thread id))

(defmethod inform-reassign ((obs <calisp-io-int-observer>) effectors src dest)
  (format (str obs) "~&Effectors ~a were reassigned from thread ~a to thread ~a."
	  effectors src dest))

(defmethod inform-end-choice-block ((obs <calisp-io-int-observer>) id omega)
  (declare (ignore omega))
  (format (str obs) "~&Thread ~a exiting choice block." id))

(defmethod inform-die-thread ((obs <calisp-io-int-observer>) current-thread)
  (format (str obs) "~&Thread ~a about to die." current-thread))
  





