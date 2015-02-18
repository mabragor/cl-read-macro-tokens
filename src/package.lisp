;;;; package.lisp

(in-package #:cl-user)

(defpackage #:cl-read-macro-tokens
  (:use #:cl #:defmacro-enhance
        #+sbcl #:sb-impl
        #+cmucl #:lisp
        #+ccl #:ccl)
  (:export #:enable-read-macro-tokens #:disable-read-macro-tokens #:with-read-macro-tokens
           #:define-read-macro #:undefine-read-macro #:with-no-read-macro-tokens #:with-no-read-macro-tokens1
           #:read-list-new
           #:read-list-old
           #:*read-macro-tokens*
           #:defmacro!! #:read-macrolet #:with-macro-character #:with-dispatch-macro-character
	   #:set-macro-token-reader))


