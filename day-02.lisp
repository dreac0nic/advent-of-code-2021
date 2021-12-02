;;;; day-02.lisp

(in-package #:day-02)

(defvar *example-input* '("forward 5"
                          "down 5"
                          "forward 8"
                          "up 3"
                          "down 8"
                          "forward 2"))

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (split "\\n" (get-puzzle-input 2)))


;;; Part One
(defun produce-sub-location (commands)
  "Provides the product of the position and depth of the submarine after processing a command sequence"
  (loop with depth = 0
        with position = 0
        for (action amount) in (mapcar (lambda (command-string)
                                         (let ((cmd (split " " command-string)))
                                           (list (first cmd) (read-from-string (second cmd)))))
                                       commands)
        do (cond
             ((string-equal action "forward") (incf position amount))
             ((string-equal action "up") (decf depth amount))
             ((string-equal action "down") (incf depth amount)))
        finally (return (* depth position))))


;; (produce-sub-location *puzzle-input*)
;; (submit-answer 2 (produce-sub-location *puzzle-input*))


;;; Part Two
(defun produce-advanced-sub-location (commands)
  "Provides the product of the position and depth of the submarine after processing a command sequence, taking into account aim"
  (loop with aim = 0
        with depth = 0
        with position = 0
        for (action amount) in (mapcar (lambda (command-string)
                                         (let ((cmd (split " " command-string)))
                                           (list (first cmd) (read-from-string (second cmd)))))
                                       commands)
        do (cond
             ((string-equal action "forward")
              (incf position amount)
              (incf depth (* amount aim)))

             ((string-equal action "up") (decf aim amount))
             ((string-equal action "down") (incf aim amount)))
        finally (return (* depth position))))


;; (produce-advanced-sub-location *puzzle-input*)
;; (submit-answer 2 (produce-advanced-sub-location *puzzle-input*) :part-two)
