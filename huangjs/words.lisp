(in-package :sforth)

(defword 2 + (x y)
  (+ x y))

(defword 0 bye ()
  (throw 'exit nil))

(defword 0 .s ()
  (prin1 *d-stack*)
  (terpri))

