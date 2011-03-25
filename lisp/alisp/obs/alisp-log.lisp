;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/log/alisp-log.lisp
;; Log all observations to a stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass <alisp-log> (<alisp-observer>)
  ((output-stream
    :type stream
    :reader str
    :initarg :str))
  (:documentation "Class <alisp-log>.  Has a single required initialization argument :str for the output stream.  Implements all the methods from <learning-algorithm>, and in each case just prints the information about the message to the stream."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro deflogmessage (name lambda-list)
  "deflogmessage NAME LAMBDA-LIST.  Used to define messages to a alisp-log.  For example, (deflogmessage inform-reset (s)) will expand to code that creates a method for inform-reset which prints 'inform-reset' and s to the stream.  See examples in this file."
  (with-gensyms (alg)
    `(defmethod ,name ,(cons `(,alg <alisp-log>) lambda-list)
		(format (str ,alg) "~&~%Alisp log informed of ~a" ,(string-downcase (symbol-name name)))
       ,@(loop for arg in lambda-list
	     for i from 0
	     collect `(format (str ,alg) "~&Arg ~a : ~a" ,i ,arg)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods overridden from <alisp-observer>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflogmessage inform-start-episode (s))
(deflogmessage inform-env-step (act rew to term))
(deflogmessage inform-alisp-step (omega choice))
(deflogmessage inform-start-execution ())
(deflogmessage inform-finish-execution ())
(deflogmessage inform-end-choice-block (omega))
(deflogmessage inform-part-prog-terminated (omega))
