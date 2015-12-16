
(in-package #:cl-user)

(defpackage #:cl-read-macro-tokens-patch
  (:use #:cl))
(in-package #:cl-read-macro-tokens-patch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'com-informatimago-backend *features*))

(in-package #:com.informatimago.common-lisp.lisp-reader.reader)

(defparameter *read-macro-tokens* (make-hash-table :test #'eq))

(defun read-list-new (stream ch)
  "Read-macro-hacked standard ( macro reader."
  (declare (ignore ch))
  (loop
     :with result     = (cons nil nil)
     :with last-cons  = result
     :with last-cdr-p = nil
     :with first-time = t
     :for ch = (progn (peek-char t stream nil t) (read-char stream t nil t))
     ;; :do (print `(:result ,result :last-cons ,last-cons
     ;;                      :last-cdr-p ,last-cdr-p :ch ,ch))
     :do (flet ((read-and-nconc (ch)
		  (let ((objects (nth-value 1 (read-0/1 stream t nil t nil ch '()))))

		    (when objects
		      (case last-cdr-p
			((nil)
			 (when first-time
			   (let ((custom-reader (gethash (car objects) *read-macro-tokens*)))
			     (when custom-reader
			       (return-from read-list-new (funcall custom-reader stream (car objects))))))
			 (setq first-time nil)
			 (setf (cdr last-cons) objects
			       ;; (list (first objects))
			       last-cons       (cdr last-cons)))
			((t)       (setf (cdr last-cons) (first objects)
					 last-cdr-p      :done))
			(otherwise (serror 'simple-reader-error stream
					   "illegal end of dotted list")))))))
	   (cond
	     ((char= #\) ch)
	      #-mocl (loop-finish)
	      #+mocl (if (eq last-cdr-p 't)
			 (serror 'simple-reader-error stream
				 "illegal end of dotted list")
			 (return (cdr result))))
	     ((char= #\. ch)
	      (if (token-delimiter-p (peek-char nil stream t nil t))
		  (if (eq result last-cons)
		      (serror 'simple-reader-error stream
			      "missing an object before the \".\" in a cons cell")
		      (case last-cdr-p
			((nil)     (setf last-cdr-p t))
			((t)       (serror 'simple-reader-error stream
					   "token \".\" not allowed here"))
			(otherwise (serror 'simple-reader-error stream
					   "illegal end of dotted list"))))
		  (read-and-nconc ch)))
	     (t
	      (read-and-nconc ch))
	     ))
     :finally (if (eq last-cdr-p 't)
		  (serror 'simple-reader-error stream
			  "illegal end of dotted list")
		  (return (cdr result)))))

(defun read-list-old (stream char)
  (reader-macro-left-parenthesis stream char))

;; Add ,. (the destructive splicing) -- which is part of the standard
(defun reader-macro-comma (stream ch)
  "Standard , macro reader."
  (declare (ignore ch))
  (format t "hello~%")
  (list (let ((char (peek-char nil stream t nil t)))
	  (cond ((char= #\@ char) (read-char stream t nil t) 'splice)
		((char= #\. char) (read-char stream t nil t) 'dsplice) ; destructive-splice
		(t 'unquote)))
	(read stream t nil t)))

;; Already here we substitute lisp reader with Zach's one

(defmacro intern-function-to-cl (name)
  `(setf (symbol-function ',(intern (string name) "CL-USER"))
	 #',name))

(defmacro intern-functions-to-cl (&rest names)
  `(progn ,@(mapcar (lambda (x) `(intern-function-to-cl ,x)) names)))

;; (defmacro intern-symbol-to-cl (name)
;;   `(setf ,(intern (string name) "CL-USER")
;; 	 ,name))

(defmacro intern-symbol-to-cl (name)
  `(shadowing-import ',name "CL-USER"))

(defmacro intern-symbols-to-cl (&rest names)
  `(progn ,@(mapcar (lambda (x) `(intern-symbol-to-cl ,x)) names)))

(intern-functions-to-cl copy-readtable
			make-dispatch-macro-character
			read read-preserving-whitespace
			read-delimited-list
			read-from-string
			readtable-case readtablep
			set-dispatch-macro-character get-dispatch-macro-character
			set-macro-character get-macro-character
			set-syntax-from-char)

(intern-symbols-to-cl ;; readtable ; a class-name, not a variable
		      ;; with-standard-io-syntax ; I'm not sure it will work this simple with macros...
		      *read-base* *read-default-float-format* *read-eval*
		      *read-suppress* *readtable*)
(setf *readtable* (copy-readtable nil))

(export '(read-list-new read-list-old *read-macro-tokens*))
