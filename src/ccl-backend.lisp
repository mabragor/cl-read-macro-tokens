
(in-package "CCL")

(defparameter *read-macro-tokens* (make-hash-table :test #'eq))

(defun pre-read-list-new (stream &optional nodots (termch #\)))
  (let* ((dot-ok (cons nil nil))
         (head (cons nil nil))
         (tail head)
         (source-note-list nil))
    (declare (dynamic-extent dot-ok head)
             (list head tail))
    (if nodots (setq dot-ok nil))
    (multiple-value-bind (firstform firstform-p firstform-source-note)
        (%read-list-expression stream dot-ok termch)
      (when firstform-source-note
        (push firstform-source-note source-note-list))
      (when firstform-p
        (if (and dot-ok (eq firstform dot-ok))       ; just read a dot
            (signal-reader-error stream "Dot context error."))
        (let ((custom-reader (gethash firstform *read-macro-tokens*)))
                (if custom-reader
                    (return-from pre-read-list-new (funcall custom-reader stream firstform))))
        (rplacd tail (setq tail (cons firstform nil)))
        (loop
          (multiple-value-bind (nextform nextform-p nextform-source-note)
              (%read-list-expression stream dot-ok termch)
            (when nextform-source-note
              (push nextform-source-note source-note-list))
            (if (not nextform-p) (return))
            (if (and dot-ok (eq nextform dot-ok))    ; just read a dot
                (if (multiple-value-bind (lastform lastform-p lastform-source-note)
                        (%read-list-expression stream nil termch)
                      (when lastform-source-note
                        (push lastform-source-note source-note-list))
                      (and lastform-p
                           (progn (rplacd tail lastform)
                                  (not (nth-value 1 (%read-list-expression stream nil termch))))))
                    (return)
                    (signal-reader-error stream "Dot context error1."))
              (rplacd tail (setq tail (cons nextform nil))))))))
    (values (cdr head) source-note-list)))

(defun read-list-new (stream char)
  (declare (ignore char))
  (pre-read-list-new stream nil #\)))

(defun read-list-old (stream char)
  (declare (ignore char))
  (read-list stream nil #\)))

(export '(read-list-new read-list-old *read-macro-tokens*))
