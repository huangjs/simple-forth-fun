;;;; leeforth.lisp

(in-package #:leeforth)

(defparameter *stack* nil)
(defparameter *dict* (make-hash-table :test #'equal))

(defun f-push (x)
  (push x *stack*))
(defun f-pop ()
  (pop *stack*))

(defmacro define-word (name &rest body)
  (let ((tmp-fn (gensym)))
    `(progn
       (defun ,tmp-fn ()
         ,@body
         )
       (setf (gethash ,name *dict*) #',tmp-fn))))

;; 数学计算

;; n n - n
(define-word "-" (f-push 
                  (let ((n1 (f-pop)))
                    (- (f-pop) n1))))

(define-word "+" (f-push (+ (f-pop) (f-pop))))


(defun eval-forth (s)
  (cond ((equal s ".s") (format nil "~A" *stack*))
        ((equal s ".") (pop *stack*))
        
        (t "ok")))

(defun forth-repl ()
  (loop (print (eval-forth (read)))))

