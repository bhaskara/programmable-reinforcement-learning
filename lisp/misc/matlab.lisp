;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matlab.lisp
;; utilities for converting to and from matlab format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package utils)

(defun matlab-write (filename seq &optional overwrite)
  "matlab-write F S &OPTIONAL (OVERWRITE NIL).  Writes sequence S to file F in matlab-readable (ascii) form.  If OVERWRITE is true, then overwrite F if it already exists."
  (flet ((write-it (f)
		  (map nil 
		    (lambda (x) (format f "~a " (float x)))
		    seq)))
    (if overwrite
	(with-open-file (f filename :direction :output :if-exists :supersede)
	  (write-it f))
      (with-open-file (f filename :direction :output)
	(write-it f)))))

(defun matlab-read (filename)
  "matlab-read FILENAME.  Read a vector from a file in matlab ascii format."
  (with-open-file (f filename :direction :input)
    (loop
	with v = (make-array 10 :adjustable t :fill-pointer 0)
	for x = (read f nil)
	while x
	when x do (vector-push-extend x v)
	finally (return v))))
	      
	     
(defun convert-to-matlab (infile &optional outfile)
  "convert-to-matlab INFILE &optional OUTFILE.  Open file INFILE, read in a single object (which must be an sequence of reals), and write it out in a matlab-readable (ascii) format OUTFILE, which is equal to INFILE by default.  Overwrite OUTFILE if it already exists."
  (with-open-file (fin infile :direction :input)
    (matlab-write (or outfile infile) (read fin) t)))

(defun convert-from-matlab (infile &optional outfile)
  "convert-from-matlab INFILE &optional OUTFILE.  Open matlab ascii file INFILE, read a sequence of numbers from it into a Lisp vector, and write out in lisp format to OUTFILE (which is equal to INFILE by default).  Overwrite OUTFILE if it already exists."
  (let ((v (matlab-read infile)))
    (with-open-file (f (or outfile infile) :direction :output :if-exists :supersede)
      (format f "~a" v))))
      

(in-package cl-user)
