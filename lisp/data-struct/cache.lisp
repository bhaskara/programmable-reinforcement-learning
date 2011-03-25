
(defpackage cache
  (:documentation "Functions to do with caches.

Types
-----
<fifo-cache>

Operations
----------
add
lookup
reset
reset-stats
size")
  (:use
   utils
   cl)
  (:export
   <fifo-cache>
   add
   size
   reset-stats
   reset
  lookup))

(in-package cache)

(defclass <fifo-cache> ()
  ((v :type (simple-array * 1) :reader v :writer set-v)
   (test :type function :initarg :test :initform #'equalp :reader test)
   (size :initarg :size :type fixnum :reader size)
   (search-ptr :type fixnum :initform 0 :accessor search-ptr)
   (ptr :type fixnum :initform 0 :accessor cache-ptr))
  (:documentation 
   "A simple implementation of FIFO caches.  Create using make-instance.

Conceptually, uses a circular array to store the items and looks them up by searching through the array. 

Initargs
:size - size of cache.  Required.
:test - test for equality.  Optional, #'equalp by default."))

(defvar *hits* 0)
(defvar *misses* 0)

(defmethod reset ((c <fifo-cache>))
  (fill (v c) nil)
  (setf (cache-ptr c) 0)
  (values))

(defmethod initialize-instance :after ((c <fifo-cache>) &rest args 
				       &key (size nil))
  (assert (integerp size))
  (set-v (make-array size) c))

(defun add (item val c)
  (setf (svref (v c) (cache-ptr c)) (cons item val)
	(search-ptr c) (cache-ptr c))
  (if (>= (cache-ptr c) (1- (size c)))
      (setf (cache-ptr c) 0)
    (incf (cache-ptr c)))
  (values))

(defun lookup (item c)
  (let* ((v (v c))
	 (pos
	  ;; search starting from search-ptr 
	  (or (position item v :test (test c) :key #'car :start (search-ptr c))
	      (position item v :test (test c) :key #'car :end (search-ptr c)))))
    (if pos
	(let ((val (cdr (svref v pos))))
	  (incf *hits*)
	  (setf (search-ptr c) pos) ;; set the search-ptr to pos
	  (values val t))
      (progn (incf *misses*) (values nil nil)))))
    

(defun reset-stats ()
  (setf *hits* 0
	*misses* 0))