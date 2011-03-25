(in-package alisp)

(defclass <alisp-progress-printer> (<alisp-observer> progress-printer:<progress-printer>)
  ())


(defun make-alisp-progress-printer (step-inc ep-inc &key (exec-notify nil) (str t))
  "make-alisp-progress-printer STEP-INC EP-INC &key (exec-notify nil) (STR t).  See <progress-printer> for details."
  (make-instance '<alisp-progress-printer> :step-inc step-inc :episode-inc ep-inc :str str :exec-notify exec-notify))
