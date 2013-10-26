(in-package #:cl-read-macro-tokens-tests)

(defmacro!! nihilling-arrays (&body body)
    (let ((*readtable* (copy-readtable))
          (stash-function (get-dispatch-macro-character #\# #\()))
      (set-dispatch-macro-character #\# #\(
                                    (lambda (stream char subchar)
                                      (funcall stash-function
                                               stream char subchar)
                                      nil))
      (format t "Im in NIHILLING-ARRAYS.~%")
      (call-next-method))
  `(progn ,@body))
      
(defmacro!! nihilling-read-eval (&body body)
    (let ((*readtable* (copy-readtable))
          (stash-function (get-dispatch-macro-character #\# #\.)))
      (set-dispatch-macro-character #\# #\.
                                    (lambda (stream char subchar)
                                      (funcall stash-function
                                               stream char subchar)
                                      nil))
      (format t "Im in NIHILLING-READ-EVAL.~%")
      (call-next-method))
  `(progn ,@body))

(defmacro!! nihilling-chars-and-strings (&body body)
    (let ((*readtable* (copy-readtable))
          (char-reader (get-dispatch-macro-character #\# #\\))
          (string-reader (get-macro-character #\")))
      (set-dispatch-macro-character #\# #\\
                                    (lambda (stream char subchar)
                                      (funcall char-reader stream char subchar)
                                      nil))
      (set-macro-character #\" (lambda (stream char)
                                 (funcall string-reader stream char)
                                 nil))
      (read-macrolet ((literal-char (lambda (stream token)
                                      (with-dispatch-macro-character (#\# #\\ char-reader)
                                        (car (read-list-old stream token)))))
                      (literal-string (lambda (stream token)
                                        (with-macro-character (#\" string-reader)
                                          (car (read-list-old stream token))))))
        (format t "Im in NIHILLING-CHARS-AND-STRINGS.~%")
        (call-next-method)))
  `(progn ,@body))
    
