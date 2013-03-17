(in-package :sforth)

(defword 2 + (x y)
  (+ x y))

(defword 2 - (x y)
  (- x y))

(defword 0 bye ()
  (throw 'exit nil))

(defword 0 .s ()
  (prin1 *d-stack*)
  (terpri))

(defword 0 pop ()
  (pop *d-stack*))

(defword 1 push (x)
  ;; FIXME:
  (push x *d-stack*))

