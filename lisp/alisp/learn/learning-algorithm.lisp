(in-package alisp)

(defclass <alisp-learning-algorithm> (<alisp-observer> <learning-algorithm>)
  ()
  (:documentation "A <alisp-learning-algorithm> is just a particular type of <alisp-observer> that learns a policy (or Q-function or model) from experience in the environment."))


(defclass <alisp-model-learning-algorithm> (<alisp-learning-algorithm>)
  ()
  (:documentation "An <alisp-model-learning-algorithm> implements a method for get-smdp."))

(defgeneric get-smdp (alg)
  (:documentation "get-smdp ALG.  Return the current SMDP estimate for this algorithm."))