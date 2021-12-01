;;;; advent-of-code-2021.lisp

(in-package #:advent-of-code-2021)

(defvar *protocol* "https")
(defvar *domain* "adventofcode.com")
(defvar *year* "2021")


(defun slurp (path)
  "Reads in entire file specified by path as a string"
  (with-open-file (stream path)
    (let ((buffer (make-string (file-length stream))))
      (read-sequence buffer stream)
      (string-trim '(#\Space #\Newline #\Tab) buffer))))


(defun base-endpoint ()
  "Contructs the base endpoint for the current year of Advent of Code"
  (format nil "~a://~a/~a" *protocol* *domain* *year*))


(defun get-days-puzzle-input (day-number)
  "Retrieves the user's puzzle input for the day specified. Day should be provided as a single integer representing taht day."
  (string-trim '(#\Space #\Newline #\Tab)
               (nth-value 0
                          (dexador:get (format nil "~a/day/~a/input" (base-endpoint) day-number)
                                       :cookie-jar (let ((cookies (make-cookie-jar)))
                                                     (-<>> (or (sb-ext:posix-getenv "AOC_SESSION")
                                                               (slurp ".session"))
                                                           (make-cookie :name "session"
                                                                        :value <>
                                                                        :domain ".adventofcode.com"
                                                                        :secure-p 't
                                                                        :httponly-p 't
                                                                        :path "/")
                                                           list
                                                           (merge-cookies cookies))
                                                     cookies)))))


(defun submit-days-answer (day-number answer &optional part)
  "Submits the given answer for the day specified. Optionally :part-one and :part-two can be specified."
  (elt (lquery:$ (initialize (dexador:post (format nil "~a/day/~a/answer" (base-endpoint) day-number)
                                           :cookie-jar (let ((cookies (make-cookie-jar)))
                                                         (-<>> (or (sb-ext:posix-getenv "AOC_SESSION")
                                                                   (slurp ".session"))
                                                               (make-cookie :name "session"
                                                                            :value <>
                                                                            :domain ".adventofcode.com"
                                                                            :secure-p 't
                                                                            :httponly-p 't
                                                                            :path "/")
                                                               list
                                                               (merge-cookies cookies))
                                                         cookies)
                                           :content `(("level" . ,(format nil "~a" (cond
                                                                                     ((eq part :part-one) 1)
                                                                                     ((eq part :part-two) 2)
                                                                                     ('t 1))))
                                                      ("answer" . ,(format nil "~a" answer)))))
                 "body main article p"
                 (render-text))
       0))
