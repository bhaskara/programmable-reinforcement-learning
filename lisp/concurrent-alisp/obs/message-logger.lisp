
(defpackage message-logger
  (:documentation "
Types
-----
<calisp-message-logger>

Operations
----------
get-log")
  (:use calisp-obs
	cl
	circ-vec
	utils)
  (:export 
   <calisp-message-logger>
   get-log))

(in-package message-logger)

(defclass <calisp-message-logger> (<calisp-observer>)
  ((log-max-length :initform 1000 :type fixnum :reader log-max-length :initarg :log-max-length)
   (msg-log :accessor msg-log))
  (:documentation "Class <calisp-message-logger>.  Subclass of <calisp-observer>.
Initargs
:log-max-length - the max length that the message log can get.  After this point, old messages will start to be removed.  Defaults to 1000."))

(defmethod inform-start-execution ((alg <calisp-message-logger>))
  (setf (msg-log alg) (make-circular-vector (log-max-length alg))))

(defun get-log (alg)
  "get-log MESSAGE-LOGGER.  Return the current log of messages"
  (get-flat-vector (msg-log alg)))
		

(defmacro def-log-method (method-name arg-list)
  (with-gensyms (alg)
    `(defmethod ,method-name ((,alg <calisp-message-logger>) ,@arg-list)
       (circular-push (list ',method-name ,@arg-list)
		      (msg-log ,alg)))))

(def-log-method inform-start-episode (s))
(def-log-method inform-env-step (a to rew term))
(def-log-method inform-part-prog-terminated ())
(def-log-method inform-calisp-step (omega u))
(def-log-method inform-spawn-thread (id current eff))
(def-log-method inform-die-thread (id))
(def-log-method inform-reassign (eff src dest))
(def-log-method inform-end-choice-block (current-thread omega))
