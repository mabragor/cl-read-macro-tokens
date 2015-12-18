
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


;;; the actual character macro
(defun backquote-macro (stream ignore)
  (declare (ignore ignore))
  (let ((*backquote-count* (1+ *backquote-count*)))
    (multiple-value-bind (flag thing)
        (backquotify stream (read stream t nil t))
      (when (eq flag *bq-at-flag*)
        (simple-reader-error stream ",@ after backquote in ~S" thing))
      (when (eq flag *bq-dot-flag*)
        (simple-reader-error stream ",. after backquote in ~S" thing))
      (backquotify-1 flag thing))))

(defun comma-macro (stream ignore)
  (declare (ignore ignore))
  (unless (> *backquote-count* 0)
    (when *read-suppress*
      (return-from comma-macro nil))
    (simple-reader-error stream *bq-error*))
  (let ((c (read-char stream))
        (*backquote-count* (1- *backquote-count*)))
    (flet ((check (what)
             (let ((x (peek-char t stream t nil t)))
               (when (and (char= x #\)) (eq #'read-right-paren (get-macro-character #\))))
                 ;; Easier to figure out than an "unmatched parenthesis".
                 (simple-reader-error stream "Trailing ~A in backquoted expression." what)))))
      (cond ((char= c #\@)
             (check "comma-at")
             (cons *bq-at-flag* (read stream t nil t)))
            ((char= c #\.)
             (check "comma-dot")
             (cons *bq-dot-flag* (read stream t nil t)))
            (t
             (unread-char c stream)
             (check "comma")
             (cons *bq-comma-flag* (read stream t nil t)))))))


;;;
(defun expandable-backq-expression-p (object)
  (and (consp object)
       (let ((flag (car object)))
         (or (eq flag *bq-at-flag*)
             (eq flag *bq-dot-flag*)))))

(defun backquote-splice (method dflag a d what stream)
  (cond (dflag
         (values method
                 (cond ((eq dflag method)
                        (cons a d))
                       (t (list a (backquotify-1 dflag d))))))
        ((expandable-backq-expression-p a)
         (values method (list a)))
        ((not (and (atom a) (backq-constant-p a)))
         ;; COMMA special cases a few constant atoms, which
         ;; are illegal in splices.
         (comma a))
        (t
         (simple-reader-error stream "Invalid splice in backquote: ~A~A" what a))))

;;; This does the expansion from table 2.
(defun backquotify (stream code)
  (cond ((atom code)
         (cond ((null code) (values nil nil))
               ((or (consp code)
                    (symbolp code))
                ;; Keywords are self-evaluating. Install after packages.
                (values 'quote code))
               (t (values t code))))
        ((or (eq (car code) *bq-at-flag*)
             (eq (car code) *bq-dot-flag*))
         (values (car code) (cdr code)))
        ((eq (car code) *bq-comma-flag*)
         (comma (cdr code)))
        ((eq (car code) *bq-vector-flag*)
         (multiple-value-bind (dflag d) (backquotify stream (cdr code))
           (values 'vector (backquotify-1 dflag d))))
        (t (multiple-value-bind (aflag a) (backquotify stream (car code))
             (multiple-value-bind (dflag d) (backquotify stream (cdr code))
               (when (eq dflag *bq-at-flag*)
                 ;; Get the errors later.
                 (simple-reader-error stream ",@ after dot in ~S" code))
               (when (eq dflag *bq-dot-flag*)
                 (simple-reader-error stream ",. after dot in ~S" code))
               (cond
                ((eq aflag *bq-at-flag*)
                 (backquote-splice 'append dflag a d ",@" stream))
                ((eq aflag *bq-dot-flag*)
                 (backquote-splice 'nconc dflag a d ",." stream))
                ((null dflag)
                 (if (member aflag '(quote t nil))
                     (values 'quote (list a))
                     (values 'list (list (backquotify-1 aflag a)))))
                ((member dflag '(quote t))
                 (if (member aflag '(quote t nil))
                     (values 'quote (cons a d ))
                     (values 'list* (list (backquotify-1 aflag a)
                                          (backquotify-1 dflag d)))))
                (t (setq a (backquotify-1 aflag a))
                   (if (member dflag '(list list*))
                       (values dflag (cons a d))
                       (values 'list*
                               (list a (backquotify-1 dflag d)))))))))))


(defun backq-constant-p (x)
  (or (numberp x) (eq x t)))

;;; This handles the <hair> cases.
(defun comma (code)
  (cond ((atom code)
         (cond ((null code)
                (values nil nil))
               ((backq-constant-p code)
                (values t code))
               (t
                (values *bq-comma-flag* code))))
        ((and (eq (car code) 'quote)
              (not (expandable-backq-expression-p (cadr code))))
         (values (car code) (cadr code)))
        ((member (car code) '(append list list* nconc))
         (values (car code) (cdr code)))
        ((eq (car code) 'cons)
         (values 'list* (cdr code)))
        (t (values *bq-comma-flag* code))))

;;; This handles table 1.
(defun backquotify-1 (flag thing)
  (cond ((or (eq flag *bq-comma-flag*)
             (member flag '(t nil)))
         thing)
        ((eq flag 'quote)
         (list  'quote thing))
        ((eq flag 'list*)
         (cond ((and (null (cddr thing))
                     (not (expandable-backq-expression-p (car thing)))
                     (not (expandable-backq-expression-p (cadr thing))))
                (cons 'backq-cons thing))
               ((expandable-backq-expression-p (car (last thing)))
                (list 'backq-append
                      (cons 'backq-list (butlast thing))
                      ;; Can it be optimized further? -- APD, 2001-12-21
                      (car (last thing))))
               (t
                (cons 'backq-list* thing))))
        ((eq flag 'vector)
         (list 'backq-vector thing))
        (t (cons (ecase flag
                   ((list) 'backq-list)
                   ((append) 'backq-append)
                   ((nconc) 'backq-nconc))
                 thing))))


;; (defun simple-reader-error (stream format-string &rest format-args)
;;   (bug "READER-ERROR on stream ~S: ~?" stream format-string format-args))

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
