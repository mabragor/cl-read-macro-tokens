
(in-package :cl-read-macro-tokens-tests)

(def-suite read-macro-tokens)
(in-suite read-macro-tokens)

(defun run-tests ()
  (let ((results (run 'read-macro-tokens)))
    (explain! results)
    (unless (results-status results)
      (error "Tests failed."))))

(enable-read-macro-tokens)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-foo (stream token)
    (read-list-old stream token))
  (setf (gethash 'foo *read-macro-tokens*) #'read-foo))

(test simple
  (is (equal '(1 2 3)
             '(foo 1 2 3)))
  (is (equal (list 'foo 1 2 3)
             (with-no-read-macro-tokens
                 '(foo 1 2 3))))
  (is (equal (list 1 (list 'progn 'foo 2) 3)
             '(foo 1 (with-no-read-macro-tokens foo 2) 3)))
  (is (equal (list 1 (list 'foo 2) 3)
             '(foo 1 (with-no-read-macro-tokens1 foo 2) 3))))

  
             
(disable-read-macro-tokens)

(test simple-wo-tokens
  (is (equal (list 'foo 1 2 3)
             '(foo 1 2 3)))
  (is (equal (list 'foo 1 2 3)
             (with-no-read-macro-tokens
                 '(foo 1 2 3)))))
