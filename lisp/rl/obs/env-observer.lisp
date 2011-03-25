(defpackage env-observer
  (:documentation "Package env-observer.  Defines <env-observer>, a type of observer that prints out what happens in the environment to a stream.

Exports
-------
<env-observer>
make-env-observer")
  (:use cl
	rl-obs
	utils)
  (:export 
   <env-observer>
   make-env-observer))

(in-package env-observer)
  
(defclass <env-observer> (<rl-observer>)
  ((output-stream :type stream
		  :reader str
		  :initarg :str
		  :writer set-output-stream
		  :initform t)
   (prompt-after-actions :type boolean
			 :accessor prompt?
			 :initarg :prompt?)
   (prompt-this-episode :type boolean
			:accessor prompt-this-ep)
   (eps :type fixnum
	:accessor eps
	:initform 0)
   (steps :type fixnum
	  :accessor steps))
  (:documentation "Subclass of <alisp-observer> whose sole function is to show what happens in the environment as a result of running an ALisp program.  Create instances using make-env-observer"))




(defun make-env-observer (&optional (stream t) (prompt? nil))
  "make-env-observer &optional (OUTPUT-STREAM t) (PROMPT? nil).  Make an <env-observer>.  When PROMPT? is true, prompts user to press enter after each step."
  (make-instance '<env-observer> :str stream :prompt? prompt? ))

(defmethod inform-start-episode ((obs <env-observer>) s)
  (awhen (str obs)
	 (format it "~&~%Environment reset to ~&~a" s))
  (setf (prompt-this-ep obs) t
	(steps obs) 0))
  

(defmethod inform-env-step ((obs <env-observer>) act rew to term)
  (awhen (str obs)
	 (format it "~&Episode ~a Step ~a.  Did ~a, received ~a, and now in state ~&~a" 
		 (eps obs) (steps obs) act rew to)
	 (when (and (prompt? obs) (prompt-this-ep obs))
	   (let ((c (prompt "~&Press enter to continue or \"skip\" to skip to end of this episode. ")))
	     (when (equal c "skip") (setf (prompt-this-ep obs) nil)))))
  (incf (steps obs))
  (when term
    (awhen (str obs)
	   (format it "~&Episode complete."))
    (incf (eps obs)))
  (values))

  
  