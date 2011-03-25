;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/alisp-observer.lisp
;; Defines the <alisp-observer> abstract class and some generic functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <alisp-observer> (<rl-observer>)
  ()
  (:documentation "Class <alisp-observer>.  Subclass of rl:<rl-observer>.  See alisp-observer package for documentation."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kinds of messages that can be sent to an alisp-observer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmessage inform-alisp-step (omega choice)
  "inform-alisp-step OBSERVER OMEGA CHOICE.  Inform observer that we have just left state OMEGA, by choosing CHOICE.  This message is sent in the following situations

1) An action statement.  In this case, CHOICE is the action.

2) A call statement.  There are then 3 subcases:
2a) If all arguments are specified for the call, CHOICE is always 'no-choice
2b) If there is a single unspecified argument, CHOICE holds the value that was chosen for that argument
2c) If there are multiple unspecified arguments, CHOICE is a list of their values.

3) A with-choice statement with variable C.  In this case, CHOICE is the value that was chosen for C.

4) A choose statement.  In this case, CHOICE is a nonnegative integer, holding the index of the form that has been chosen.
")

(defmessage inform-end-choice-block (omega)
  "inform-end-choice-block OBSERVER OMEGA.  Inform observer that the most recent choice block has been exited.")


(defmessage inform-part-prog-terminated (omega)
  "inform-part-prog-terminated OBSERVER.  Inform observer that the partial program has terminated.")


