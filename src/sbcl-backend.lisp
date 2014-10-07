
(in-package "SB-IMPL")

(defparameter *read-macro-tokens* (make-hash-table :test #'eq))

(defun read-list-new (stream ignore)
  (declare (ignore ignore))
  (let* ((thelist (list nil))
         (listtail thelist)
	 (first-time t))
    (declare (dynamic-extent thelist))
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
                 (rplacd listtail (read-after-dot stream nextchar))
                 ;; Check for improper ". ,@" or ". ,." now rather than
                 ;; in the #\` reader. The resulting QUASIQUOTE macro might
                 ;; never be exapanded, but nonetheless could be erroneous.
                 (when (and (plusp *backquote-depth*) (not *read-suppress*))
                   (let ((lastcdr (cdr (last listtail))))
                     (when (and (comma-p lastcdr) (comma-splicing-p lastcdr))
                       (simple-reader-error
                        stream "~S contains a splicing comma after a dot"
                        (cdr thelist)))))
                 (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (multiple-value-bind (winp obj) (read-maybe-nothing stream firstchar)
        ;; allows the possibility that a comment was read
        (when winp
          (if first-time
              (let ((custom-reader (gethash obj *read-macro-tokens*)))
                (if custom-reader
                    (return-from read-list-new (funcall custom-reader stream obj)))))
          (setq listtail (cdr (rplacd listtail (list obj))))
	  (setq first-time nil))))))

(defun read-list-old (stream char)
  (read-list stream char))

(export '(read-list-new read-list-old *read-macro-tokens*))
