;;;; package.lisp

(in-package #:cl-user)

(defpackage #:cl-read-macro-tokens
  (:use #:cl
        #+sbcl #:sb-impl
        #+cmucl #:lisp)
  (:export #:enable-read-macro-tokens #:disable-read-macro-tokens #:with-read-macro-tokens
           #:define-read-macro #:undefine-read-macro #:with-no-read-macro-tokens #:with-no-read-macro-tokens1
           #:read-list-new
           #:read-list-old
           #:*read-macro-tokens*))


