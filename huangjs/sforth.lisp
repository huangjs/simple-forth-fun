;;;; sforth.lisp

(in-package #:sforth)

(declaim (optimize debug))

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
  (let ((eof (load-time-value (gensym "EOF"))))
    (iter (for (values e i) = (read-from-string line nil eof :start (or i 0)))
          (until (eq e eof))
          (dispatch e))
    *d-stack*))

(defun pe-interpret (line)
  (let ((eof (load-time-value (gensym "EOF"))))
    (iter (for (values e i) = (read-from-string line nil eof :start (or i 0)))
          (until (eq e eof))
          (dispatch e))
    *d-stack*))

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
     (macrolet ((f-push (exp)
                  (alexandria:once-only (exp)
                    `(progn
                       (push ,exp *d-stack*)
                       ,exp))))
       (let ((fun (lambda ,args
                    #+debug (print (list ',name ,@args))
                    ,@body)))
         (setf (gethash ',name *words*)
               (list ,nargs fun))))))

(defun reset ()
  (setf *d-stack* nil))

(defun load-file (filename &key compilep)
  (with-open-file (f filename)
    (let ((lines (make-string (file-length f))))
      (read-sequence lines f)
      (if compilep
          (funcall (compile nil `(lambda () ,(pe-interpret lines))))
          (interpret lines)))))
