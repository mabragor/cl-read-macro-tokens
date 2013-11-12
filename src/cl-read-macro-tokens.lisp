;;;; cl-read-macro-tokens.lisp

(in-package #:cl-read-macro-tokens)

(defmacro define-read-macro (name &body body)
  (let ((fname (gensym (string name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,fname (,(intern "STREAM") ,(intern "TOKEN"))
         ,@body)
       (setf (gethash ',name *read-macro-tokens*) #',fname))))

(defmacro undefine-read-macro (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (remhash ',name *read-macro-tokens*)))

(defun %enable-read-macro-tokens (&optional (readtable *readtable*))
  (set-macro-character #\( #'read-list-new nil readtable)
  (setf (gethash 'with-no-read-macro-tokens *read-macro-tokens*)
        #'%with-no-read-macro-tokens)
  (setf (gethash 'with-no-read-macro-tokens1 *read-macro-tokens*)
        #'%with-no-read-macro-tokens1)
  (values))
  

(defun %disable-read-macro-tokens (&optional (readtable *readtable*))
  (set-macro-character #\( #'read-list-old nil readtable)
  (undefine-read-macro with-no-read-macro-tokens)
  (values))

(defmacro enable-read-macro-tokens (&optional (readtable '*readtable*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-read-macro-tokens ,readtable)))

(defmacro disable-read-macro-tokens (&optional (readtable '*readtable*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-read-macro-tokens ,readtable)))

(defmacro with-read-macro-tokens ((&optional (readtable '*readtable*)) &body body)
  `(unwind-protect (progn (%enable-read-macro-tokens ,readtable)
                          ,@body)
     (%disable-read-macro-tokens ,readtable)))

;; The idea here is, that when READ-MACRO-TOKENS are turned on, macroexpander
;; never sees any WITH-NO-READ-MACRO-TOKENS, since already reader takes care
;; of them.
;; When READ-MACRO-TOKENS are turned off, however, we need, that code
;; still made some sense - hence need  a definition of a macro WITH-NO-READ-MACRO-TOKENS.
(defun %with-no-read-macro-tokens (stream token)
  (let ((*read-macro-tokens* (make-hash-table :test #'eq)))
    `(progn ,@(read-list-old stream #\())))
(defmacro with-no-read-macro-tokens (&body body)
  (warn "WITH-NO-READ-MACRO-TOKENS appeared in context, where READ-MACRO-TOKENS are disabled. Is this right?")
  `(progn ,@body))
(defun %with-no-read-macro-tokens1 (stream token)
  (let ((*read-macro-tokens* (make-hash-table :test #'eq)))
    (read-list-old stream #\()))
(defmacro with-no-read-macro-tokens1 (&body body)
  (error "WITH-NO-READ-MACRO-TOKENS1 appeared in context, where READ-MACRO-TOKENS are disabled.
This is definitely not right!")
  `(progn ,@body))
