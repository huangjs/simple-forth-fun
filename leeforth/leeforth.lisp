;;;; leeforth.lisp

(in-package #:leeforth)

(defparameter *stack* nil)
(defparameter *dict* (make-hash-table :test #'equal))

(defun f-push (x)
  (push x *stack*))
(defun f-pop ()
  (pop *stack*))

(defun ff-+ ()
  ;; n n - n
  ;; +
  (f-push (+ (f-pop) (f-pop))))
(setf (gethash "+" *dict*) #'ff-+)

(defun eval-forth (s)
  (cond ((equal s ".s") (format nil "~A" *stack*))
        ((equal s ".") (pop *stack*))
        
        (t "ok")))

(defun forth-repl ()
  (loop (print (eval-forth (read)))))

