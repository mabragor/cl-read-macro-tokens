
(in-package "SB-IMPL")

(defparameter *read-macro-tokens* (make-hash-table :test #'eq))

(defun read-list-new (stream ignore)
  (declare (ignore ignore))
  (let* ((thelist (list nil))
         (listtail thelist)
         (first-time t))
    (do ((firstchar (flush-whitespace stream) (flush-whitespace stream)))
        ((char= firstchar #\) ) (cdr thelist))
      (when (char= firstchar #\.)
            (let ((nextchar (read-char stream t)))
              (cond ((token-delimiterp nextchar)
                     (cond ((eq listtail thelist)
                            (unless *read-suppress*
                              (simple-reader-error
                               stream
                               "Nothing appears before . in list.")))
                           ((whitespace[2]p nextchar)
                            (setq nextchar (flush-whitespace stream))))
                     (rplacd listtail
                             ;; Return list containing last thing.
                             (car (read-after-dot stream nextchar)))
                     (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                    (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (let ((listobj (read-maybe-nothing stream firstchar)))
        ;; allows the possibility that a comment was read
        (when listobj
          (if first-time
              (let ((custom-reader (gethash (car listobj) *read-macro-tokens*)))
                (if custom-reader
                    (return-from read-list-new (funcall custom-reader stream (car listobj))))))
          (rplacd listtail listobj)
          (setq listtail listobj)
          (setq first-time nil))))))

(defun read-list-old (stream char)
  (read-list stream char))

(export '(read-list-new read-list-old *read-macro-tokens*))
