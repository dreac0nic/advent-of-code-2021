;;;; advent-of-code-2021.asd

(asdf:defsystem #:advent-of-code-2021
  :description "Describe advent-of-code-2021 here"
  :author "spenser bray <spenser.bray@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:arrows #:dexador #:cl-cookie #:cl-ppcre #:lquery #:cl-graph #:uiop)
  :components ((:file "package")
               (:file "utility")
               (:file "advent-of-code-2021")
               (:file "day-01")
               (:file "day-02")
               (:file "day-03")
               (:file "day-04")
               (:file "day-05")
               (:file "day-06")
               (:file "day-07")
               (:file "day-08")
               (:file "day-09")
               (:file "day-10")
               (:file "day-16")
               (:file "day-17")
               (:file "day-18")))
