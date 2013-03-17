(in-package :sforth)

(defword 2 + (x y)
  ;; (f-push (+ x y))
  (+ x y))

(defword 2 - (x y)
  ;; (f-push (- x y))
  (- x y))

(defword 0 bye ()
  (setf *d-stack* nil)
  (throw 'exit nil))

(defword 0 .s ()
  *d-stack*)

(defword 0 pop ()
  (pop *d-stack*))

(defword 0 push ()
  (push *retval* *d-stack*)
  (values))
