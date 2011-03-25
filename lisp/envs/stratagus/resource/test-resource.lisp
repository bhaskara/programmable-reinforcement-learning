(load "envs/stratagus/resource/resource-env.lisp")

(in-package resource-env)

(setf e (make-instance '<resource-env> :target-gold 500 :target-wood 100))
