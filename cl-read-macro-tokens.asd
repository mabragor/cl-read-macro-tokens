;;;; cl-read-macro-tokens.asd

(defpackage :cl-read-macro-tokens-system
  (:use :cl :asdf))

(in-package cl-read-macro-tokens-system)

(defsystem #:cl-read-macro-tokens
  :serial t
  :description "Allows reader macros to be attached to tokens, not only chars"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :pathname "src/"
  :depends-on (#:defmacro-enhance)
  :components (#+sbcl (:file "sbcl-backend")
               #+cmucl (:file "cmucl-backend")
               #+ccl (:file "ccl-backend")
               #-(or sbcl cmucl ccl)(:file "not-implemented")
               (:file "package")
               (:file "cl-read-macro-tokens")
               (:file "read-macro")))

(defsystem :cl-read-macro-tokens-tests
  :description "Tests for CL-READ-MACRO-TOKENS."
  :licence "GPL"
  :serial t
  :depends-on (:cl-read-macro-tokens :fiveam :iterate)
  :pathname "src/"
  :components ((:file "package-tests")
               (:file "macro-tests")
               (:file "macro2-tests")
               (:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-read-macro-tokens))))
  (load-system :cl-read-macro-tokens-tests)
  (funcall (intern "RUN-TESTS" :cl-read-macro-tokens-tests)))
