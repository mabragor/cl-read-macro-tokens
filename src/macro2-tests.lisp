(in-package #:cl-read-macro-tokens-tests)

(defmacro!! nihilling-arrays-successor (&body body)
  nil
  `(nihilling-arrays ,@body))

(defmacro!! nihilling-arrays-successor2 (&body body)
  nil
  `(nihilling-arrays (nihilling-arrays ,@body)))

(defmacro!! nihilling-arrays-and-read-eval (&body body)
    nil
    `(nihilling-arrays (nihilling-read-eval ,@body)))
