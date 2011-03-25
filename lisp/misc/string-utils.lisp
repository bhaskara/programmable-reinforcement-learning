(in-package utils)

(defun split-string (str &rest delims)
  "split-string STRING &rest DELIMITERS

STRING : a string
DELIMITERS : a list of characters

Split STRING into substrings separated by blocks of at least one character from DELIMITERS.  Return the substrings as multiple values."
  
  (flet
      ((is-delim (c) (member c delims))
       (is-not-delim (c) (not (member c delims))))
    (values-list
     (loop
	 with i = 0
	 with l = (length str)

	 for j = (position-if #'is-not-delim str :start i)
	 while j

	 do (setf i (or (position-if #'is-delim str :start j) l))
	 collect (subseq str j i)))))