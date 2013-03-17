;;;; sforth.lisp

(in-package #:sforth)

(declaim (optimize debug))

;;; "sforth" goes here. Hacks and glory await!

(defvar *retval* nil "Holding return value for words")
(defvar *words* (make-hash-table) "The definitions of words")
(defvar *words-code* (make-hash-table) "The definition in sexp for words, for partial evaluation")
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
  "Partial evaluator for sforth.

\(pe-interpret \"0 1 + push 2 3 - push + 4 5\")

will be compiled to 

\(LOCALLY
 (PROG1
     (PRIN1
      (FUNCALL (LAMBDA (X Y) (+ X Y))
               (LOCALLY
                (PROG1 (PRIN1 (FUNCALL (LAMBDA (X Y) (- X Y)) 3 2)) (TERPRI)))
               (LOCALLY
                (PROG1 (PRIN1 (FUNCALL (LAMBDA (X Y) (+ X Y)) 1 0))
                  (TERPRI)))))
   (TERPRI)))
"
  (let ((eof (load-time-value (gensym "EOF")))
        *code*)
    (declare (special *code*))
    (iter (for (values e i) = (read-from-string line nil eof :start (or i 0)))
          (until (eq e eof))
          (pe-dispatch e)
          (finally (return `(locally ,@*code*))))))

(defun dispatch (e)
  (typecase e
    (symbol (let ((definition (gethash e *words*)))
              (if definition
                  (run definition)
                  (format t "~&No definition for word: ~a~%" e))))
    (otherwise (push e *d-stack*))))

(defun pe-dispatch (e)
  (declare (special *code*))
  ;; optimize out push so there will be no stack usage :)
  (case e
    (push
     (push `(locally ,@*code*) *d-stack*)
     (setf *code* nil))
    ;; no stack, no states
    (.s)
    ;; general words
    (otherwise
     (typecase e
       (symbol (let ((definition (gethash e *words-code*)))
                 (if definition
                     (setf *code* (append *code* (pe-run definition)))
                     (error "~&No definition for word: ~a~%" e))))
       (otherwise (push e *d-stack*))))))

(defun run (definition)
  (destructuring-bind (nargs fun) definition
    (assert (>= (length *d-stack*) nargs))
    (let ((args (loop repeat nargs
                      collect (pop *d-stack*))))       
      (setf *retval* (prin1 (apply fun args)))
      (terpri))))

(defun pe-run (definition)
  (destructuring-bind (nargs fun) definition
    (assert (>= (length *d-stack*) nargs))
    (let ((args (loop repeat nargs
                      collect (pop *d-stack*))))       
      `((prog1
            (prin1 (funcall ,fun ,@args))
          (terpri))))))

(defmacro defword (nargs name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) 
     (macrolet ((f-push (exp)
                  (alexandria:once-only (exp)
                    `(progn
                       (push ,exp *d-stack*)
                       ,exp))))
       (let ((code '(lambda ,args ,@body))
             (fun (lambda ,args ,@body)))
         (setf (gethash ',name *words*)
               (list ,nargs fun))
         (setf (gethash ',name *words-code*)
               (list ,nargs code))))))

(defun reset ()
  (setf *d-stack* nil))

(defun load-file (filename &key compilep)
  (with-open-file (f filename)
    (let ((lines (make-string (file-length f))))
      (read-sequence lines f)
      (if compilep
          (funcall (compile nil `(lambda () (reset) ,(pe-interpret lines))))
          (interpret lines)))))
