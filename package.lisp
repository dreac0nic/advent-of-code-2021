;;;; package.lisp

(defpackage #:advent-of-code-2021
  (:use #:cl #:arrows #:cl-cookie #:lquery)
  (:export :get-puzzle-input :submit-answer))


(defpackage #:day-01
  (:use #:cl #:arrows #:advent-of-code-2021 #:cl-ppcre))
