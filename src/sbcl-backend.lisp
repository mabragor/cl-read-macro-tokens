
(in-package #:cl-user)

(defpackage #:cl-read-macro-tokens-patch
  (:use #:cl))
(in-package #:cl-read-macro-tokens-patch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun new-style-fun-type-p (lst)
    (equal '(values t t &optional) lst))
  (defun very-new-style-fun-type-p (lst)
    (equal '(values bit t &optional) lst)))
  

(eval-when (:compile-toplevel :execute)
  (let ((it (caddr (sb-impl::%fun-type #'sb-impl::read-maybe-nothing))))
    (cond ((new-style-fun-type-p it) (pushnew 'new-style-readlist *features*))
	  ((very-new-style-fun-type-p it) (pushnew 'very-new-style-readlist *features*))
	  ((equal '* it) (pushnew 'old-style-readlist *features*))
	  (t (pushnew 'not-supported-sbcl *features*)))))

(in-package "SB-IMPL")

(defparameter *read-macro-tokens* (make-hash-table :test #'eq))

#+cl-read-macro-tokens-patch::very-new-style-readlist
(macrolet
    ((with-list-reader ((streamvar delimiter) &body body)
       `(let* ((thelist (list nil))
               (listtail thelist)
               (collectp (if *read-suppress* 0 -1))
	       (first-time t))
          (declare (dynamic-extent thelist))
          (loop (let ((firstchar (flush-whitespace ,streamvar)))
                  (when (eq firstchar ,delimiter)
                    (return (cdr thelist)))
                  ,@body))))
     (read-list-item (streamvar)
       `(multiple-value-bind (winp obj)
            (read-maybe-nothing ,streamvar firstchar)
          ;; allow for a character macro return to return nothing
          (unless (zerop (logand winp collectp))
	    (if first-time
		(let ((custom-reader (gethash obj *read-macro-tokens*)))
		  (if custom-reader
		      (return-from read-list-new (funcall custom-reader stream obj)))))
            (setq listtail
                  (cdr (rplacd (truly-the cons listtail) (list obj))))
	    (setq first-time nil)))))
  ;;; The character macro handler for left paren
  (defun read-list-new (stream ignore)
    (declare (ignore ignore))
    (with-list-reader (stream #\))
      (when (eq firstchar #\.)
        (let ((nextchar (read-char stream t)))
          (cond ((token-delimiterp nextchar)
                 (cond ((eq listtail thelist)
                        (unless (zerop collectp)
                          (simple-reader-error
                           stream "Nothing appears before . in list.")))
                       ((whitespace[2]p nextchar)
                        (setq nextchar (flush-whitespace stream))))
                 (rplacd (truly-the cons listtail)
                         (read-after-dot stream nextchar collectp))
                 ;; Check for improper ". ,@" or ". ,." now rather than
                 ;; in the #\` reader. The resulting QUASIQUOTE macro might
                 ;; never be exapanded, but nonetheless could be erroneous.
                 (unless (zerop (logand *backquote-depth* collectp))
                   (let ((lastcdr (cdr (last listtail))))
                     (when (and (comma-p lastcdr) (comma-splicing-p lastcdr))
                       (simple-reader-error
                        stream "~S contains a splicing comma after a dot"
                        (cdr thelist)))))
                 (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (read-list-item stream))))


#+cl-read-macro-tokens-patch::new-style-readlist
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

#+cl-read-macro-tokens-patch::old-style-readlist
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

#+cl-read-macro-tokens-patch::not-supported-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This version of SBCL is not supported!"))

(defun read-list-old (stream char)
  (read-list stream char))

(export '(read-list-new read-list-old *read-macro-tokens*))
