(defpackage condition-variable
  (:documentation "Package for condition variables.  Condition variables are a synchronization tool typically used to make a thread wait for some state that depends on another thread's actions.

Condition variables are always associated with a process lock, and all condition variable operations must be done while a process is holding the lock that is associated with the condition variable, i.e. (eq (process-lock-locker lock) *current-process*) (and this is asserted in each method.

To use a condition variable, use these methods:

(make-condition-variable process-lock)
    Creates a condition variable associated with the given process lock.  It is perfectly acceptable to associate multiple condition variables with the same lock.

(wait condition-variable)
    Wait causes the current process to block (wait) on the condition variable.  The process gives up the lock while holding, but is guaranteed to have it again when it returns.  Wait returns true.

(notify-all condition-variable)
    Notify-all causes all processes that are blocked on the condition variable to wake up and become able to be scheduled.  Notify-all does not release the lock, so no process that has just awoken will actually run until the calling process releases the lock and a woken process is able to acquire it.  It returns a list of all processes that were woken up, if any.  Otherwise, the empty list is returned.

(notify condition-variable)
    Notify is like notify-all except that it only wakes up one (if any) process that is blocked on the condition variable.  Occasionally useful, but notify-all is more often useful, and easier to use safely.  If a process is woken up, it is returned.  Otherwise, nil is returned.

(num-waiting condition-variable)
    Returns the number of processes waiting on the condition variable.

(get-waiting condition-variable)
Returns a list of the processes waiting on this condition variable.


The code in this package currently only works with Allegro Lisp, Version 6.2 or later.")


  (:nicknames cv)
  (:use common-lisp 
	mp
	utils)
  (:export 
   <condition-variable>
   make-condition-variable
   remove-waiting-processes
   wait
   notify
   notify-all
   num-waiting
   get-waiting))
   
	
(in-package condition-variable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <condition-variable> ()
  ((lock
    :type     process-lock
    :initarg  :lock
    :reader   cv-lock)
   (waiting-processes
    :type     list
    :initform '()
    :reader   cv-get-waiting-processes
    :writer   cv-set-waiting-processes)
   (num-waiting
    :type fixnum
    :initform 0
    :accessor cv-num-waiting)
   (name
    :initarg :name
    :reader name
    :initform nil)
   ))

(defmethod make-condition-variable ((lock process-lock))
  (make-instance '<condition-variable>
    :lock lock))

(defun remove-waiting-processes (cv)
  "remove-waiting-processes CV.  Used to reset a condition variable.  Not synchronized - should not be called when there are multiple threads running."
  (cv-set-waiting-processes nil cv)
  (setf (cv-num-waiting cv) 0))


(defmethod wait ((cv <condition-variable>))
  
  
  (let ((lock (cv-lock cv)))
    (assert (eq (process-lock-locker lock) *current-process*) ()
      "Wait called without holding lock:~A" lock)
    (cv-set-waiting-processes
     (cons *current-process* (cv-get-waiting-processes cv))
     cv)
    (incf (cv-num-waiting cv))
    (unwind-protect
	(mp::without-interrupts
	  (process-unlock lock)
	  (process-disable *current-process*))
      (process-lock lock))
    
    (decf (cv-num-waiting cv))
    
    
    t))

(defmethod notify-all ((cv <condition-variable>))
  
  (let ((lock (cv-lock cv))
        (waiting (cv-get-waiting-processes cv)))
    (assert (eq (process-lock-locker lock) *current-process*) ()
      "Notify-all called without holding lock:~A" lock)
    (mapc #'process-enable waiting)
    (cv-set-waiting-processes nil cv)
    
    waiting))

(defmethod notify ((cv <condition-variable>))
  (let ((lock (cv-lock cv))
        (waiting (cv-get-waiting-processes cv)))
    (assert (eq (process-lock-locker lock) *current-process*) ()
      "Notify called without holding lock:~A" lock)
    (if waiting
      (progn
        (process-enable (first waiting))
        (cv-set-waiting-processes (rest waiting) cv)
        (first waiting))
      nil)))

(defmethod num-waiting ((cv <condition-variable>))
  (let ((lock (cv-lock cv)))
    (assert (eq (process-lock-locker lock) *current-process*) ()
      "Num-waiting called without holding lock:~A" lock)
    (cv-num-waiting cv)))

(defmethod get-waiting ((cv <condition-variable>))
  (let ((lock (cv-lock cv)))
    (assert (eq (process-lock-locker lock) *current-process*) ()
      "get-waiting called without holding lock:~A" lock)
    (cv-get-waiting-processes cv)))

;; Leave package
(in-package common-lisp-user)
