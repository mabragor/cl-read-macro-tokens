;;; This file tries to define macro, which are combination of read-macro-token and
;;; ordinary macro

(in-package cl-read-macro-tokens)

(defclass tautological-read-macro-token ()
  ((name :initform (error "Name of tautological class should be supplied")
         :initarg :name)))

(defgeneric read-handler (obj stream token)
  (:documentation "Main method to define behaviour of tautological macro-tokens.")
  (:method ((obj tautological-read-macro-token) stream token)
    `(,(slot-value obj 'name) ,@(read-list-old stream token))))

(defun read-macro-token-p (symb)
  (gethash symb *read-macro-tokens*))

(defvar *read-macro-tokens-classes* (make-hash-table :test #'eq))
(defvar *read-macro-tokens-instances* (make-hash-table :test #'eq))

(defmacro! defmacro!! (name args reader-init &body body)
  (multiple-value-bind (forms decls doc) (defmacro-enhance::parse-body body)
    (let ((readmacro-tokens (if (find 'force-tautological-reader (alexandria:flatten reader-init)
				      :test (lambda (x y)
					      (and (symbolp x) (symbolp y) (string= (string x) (string y)))))
				'(tautological-read-macro-token)
				(or (remove-duplicates
				     (remove-if-not #'identity
						    (mapcar (lambda (x)
							      (gethash x *read-macro-tokens-classes*))
							    (remove-if-not #'read-macro-token-p
									   (alexandria:flatten forms)))))
				    '(tautological-read-macro-token))))
          ;; All these under-the-hood classes names should be somewhere, right?
          ;; Originally I wanted them to be GENSYMs, but this seems to break
          ;; CLOS machinery somehow (it starts complaining about forward-referenced classes)
          (myclass (intern (string name) "CL-READ-MACRO-TOKENS")))
      ;; (format t "Compiling DEFMACRO!! name is ~a, tokens inside are ~a~%" name readmacro-tokens)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,myclass ,readmacro-tokens ())
         (defmethod read-handler :around ((,e!-obj ,myclass) ,e!-stream ,e!-token)
                    ;; Yes, this injection of OBJ, STREAM and TOKEN is intended
                    ,(if reader-init
			 `(macrolet ((force-tautological-reader ()
				       `(call-next-method)))
			    ,reader-init)
			 '(call-next-method)))
         (setf (gethash ',name *read-macro-tokens-classes*) ',myclass
               (gethash ',name *read-macro-tokens-instances*) (make-instance ',myclass
                                                                             :name ',name))
         (setf (gethash ',name *read-macro-tokens*)
               (lambda (stream token)
                 (read-handler (gethash ',name *read-macro-tokens-instances*)
                               stream token)))
         (defmacro! ,name ,args
           ,@(if doc `(,doc))
           ,@decls
           ,@body)))))

(defmacro! read-macrolet (definitions &body body)
  (if (not definitions)
      `(progn ,@body)
      (destructuring-bind (name fun) (car definitions)
        `(multiple-value-bind (,g!-it  ,g!-got) (gethash ',name *read-macro-tokens*)
           (unwind-protect (progn (setf (gethash ',name *read-macro-tokens*) ,fun)
                                  (read-macrolet ,(cdr definitions) ,@body))
             (if ,g!-got
                 (setf (gethash ',name *read-macro-tokens*) ,g!-it)
                 (remhash ',name *read-macro-tokens*)))))))

(defmacro! with-macro-character ((char fun) &body body)
  `(let ((,g!-it (get-macro-character ,char)))
     (unwind-protect (progn (set-macro-character ,char ,fun)
                            ,@body)
       (set-macro-character ,char ,g!-it))))

(defmacro! with-dispatch-macro-character ((char subchar fun) &body body)
  `(let ((,g!-it (get-dispatch-macro-character ,char ,subchar)))
     (unwind-protect (progn (set-dispatch-macro-character ,char ,subchar ,fun)
                            ,@body)
       (set-dispatch-macro-character ,char ,subchar ,g!-it))))

(defvar the-token-instance nil)
(define-symbol-macro the-token (slot-value the-token-instance 'name))

(defun random-string ()
  (coerce (let ((res nil))
	    (dotimes (i 8)
	      (push (code-char (+ 65 (random 26))) res))
	    res)
	  'string))

(defun random-new-class-name ()
  (loop (let ((class-name (intern (random-string) "CL-READ-MACRO-TOKENS")))
	  (if (not (find-class class-name nil))
	      (return class-name)))))

(defmacro! set-macro-token-reader (token reader)
  (let ((class (intern (concatenate 'string (package-name *package*) "-" (string token))
		       "CL-READ-MACRO-TOKENS")))
    `(progn (defclass ,class (tautological-read-macro-token) ())
	    (defmethod read-handler :around ((obj ,class) stream token)
	      (let ((the-token-instance obj))
		;; (format t "In read handler: ~a ~a~%" the-token-instance the-token)
		(funcall ,reader stream token)))
	    (setf (gethash ',token *read-macro-tokens-classes*)
		  ',class
		  (gethash ',token *read-macro-tokens-instances*)
		  (make-instance ',class
				 :name ',token))
	    (setf (gethash ',token *read-macro-tokens*)
		  (lambda (stream token)
		    (read-handler (gethash ',token *read-macro-tokens-instances*)
				  stream token))))))


