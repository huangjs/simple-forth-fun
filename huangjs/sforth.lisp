;;;; sforth.lisp

(in-package #:sforth)

;;; "sforth" goes here. Hacks and glory await!

(defvar *words* (make-hash-table) "The definitions of words")
(defvar *d-stack* nil "The data stack")

(defun repl ()
  (catch 'exit
    (loop for l = (read-line t nil nil)
          while l
          do (interpret l)))
  (princ 'bye)
  (values))

(defun interpret (line)
  (iter (for (values e i) = (read-from-string line nil nil :start (or i 0)))
        (while e)
        (dispatch e)))

(defun dispatch (e)
  (typecase e
    (symbol (let ((definition (gethash e *words*)))
              (if definition
                  (run definition)
                  (format t "~&No definition for word: ~a~%" e))))
    (otherwise (push e *d-stack*))))

(defun run (definition)
  (destructuring-bind (nargs fun) definition
    (assert (>= (length *d-stack*) nargs))
    (let ((args (loop repeat nargs
                      collect (pop *d-stack*))))       
      (prin1 (apply fun args))
      (terpri))))

(defmacro defword (nargs name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((fun (lambda ,args ,@body)))
       (setf (gethash ',name *words*)
             (list ,nargs fun)))))

