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

