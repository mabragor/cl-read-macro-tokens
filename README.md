cl-read-macro-tokens
====================

Why should things, that affect Lisp-reader, be limited to special characters (called macro-characters)?

This package adds possibility to define 'macro-tokens'.

Macro-token is any symbol, which happens to be in CAR-position of a list.

Example:
```lisp
CL-USER> (ql:quickload 'cl-read-macro-tokens)
CL-USER> (cl-read-macro-tokens:enable-read-macro-tokens)
CL-USER> (setf (gethash 'foo cl-read-macro-tokens:*read-macro-tokens*)
               ;; Very simple custom reader, which ignores token altogether
               (lambda (stream token)
                 (cl-read-macro-tokens:read-list-old stream token)))
CL-USER> '(foo 1 2 3)
(1 2 3)
CL-USER> (cl-read-macro-tokens:disable-read-macro-tokens)
CL-USER> '(foo 1 2 3)
(FOO 1 2 3)
;; *READ-MACRO-TOKENS* is obviously preserved across enables/disables
CL-USER> (cl-read-macro-tokens:enable-read-macro-tokens)
CL-USER> '(foo 1 2 3)
(1 2 3)
```
The package works by redefining a function, which is associated to #\( macrocharacter in the readtable.
Both ENABLE-READ-MACRO-TOKENS and DISABLE-READ-MACRO-TOKENS can be given optional READTABLE argument,
which defaults to \*READTABLE\*.

When read-macro-tokens are enabled, one can temporarily disable them, using
WITH-NO-READ-MACRO-TOKENS or WITH-NO-READ-MACRO-TOKENS1 read-macro token.

```lisp
CL-USER> (setf (gethash 'foo cl-read-macro-tokens:*read-macro-tokens*)
               ;; Very simple custom reader, which ignores token altogether
               (lambda (stream token)
                 (cl-read-macro-tokens:read-list-old stream token)))
CL-USER> '(foo 1 (cl-read-macro-tokens:with-no-read-macro-tokens (foo 2) 3))
(1 (progn (foo 2)) 3)
CL-USER> '(foo 1 (cl-read-macro-tokens:with-no-read-macro-tokens1 (foo 2) 3))
(1 (foo 2) 3)
```

The former one expands into progn-form, while the other into ordinary list-form.
Hence, former is used when defining code, and latter when defining data.

TODO:
-----

  - Make DEFINE-READ-MACRO actually work.
  - Make a convenience wrapper around DEFMACRO, which allows to define macros,
    which are at the same time read-macro-tokens.
    They should scan their body, and if they contain other such macro-read-macro-tokens,
    they should inherit read-macro-token function from them.
  - Support all major implementations
    - (done) SBCL
    - (done) CMUCL
    - ECL
    - CLISP
    - (done) Clozure