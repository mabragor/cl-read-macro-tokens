
(in-package "LISP")

(defparameter *read-macro-tokens* (make-hash-table :test #'eq))

(ext:without-package-locks
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
                          (if *read-suppress*
                              (return-from read-list-new nil)
                              (%reader-error stream _"Nothing appears before . in list.")))
                         ((whitespacep nextchar)
                          (setq nextchar (flush-whitespace stream))))
                   (rplacd listtail
                           ;;return list containing last thing.
                           (car (read-after-dot stream nextchar)))
                   (return (cdr thelist)))
                  ;;put back nextchar so we can read it normally.
                  (t (unread-char nextchar stream)))))
        ;;next thing is not an isolated dot.
        (let ((listobj (read-maybe-nothing stream firstchar)))
          ;;allows the possibility that a comment was read.
          (when listobj
            (if first-time
                (let ((custom-reader (gethash (car listobj) *read-macro-tokens*)))
                  (if custom-reader
                      (return-from read-list-new (funcall custom-reader stream (car listobj))))))
            (rplacd listtail listobj)
            (setq listtail listobj))))))

  (defun read-list-old (stream char)
    (read-list stream char))

  (export '(read-list-new read-list-old *read-macro-tokens*)))

