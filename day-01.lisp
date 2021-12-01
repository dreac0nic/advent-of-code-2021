;;;; day-01.lisp

(in-package #:day-01)

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (mapcar #'parse-integer (split "\\n" (get-days-puzzle-input 1))))

;;;; Part One
(get-days-puzzle-input 1)


;; Example
;; (find-expense-entries '(1721 979 366 299 675 1456))

;; Solution
;; (find-expense-entries *puzzle-input*)
;; (submit-days-answer 1 (find-expense-entries *puzzle-input*))


;;;; Part Two


;; Example
;; (find-triple-expense-entries '(1721 979 366 299 675 1456))

;; Solution
;; (find-triple-expense-entries *puzzle-input*)
;; (submit-days-answer 1 (find-triple-expense-entries *puzzle-input*) :part-one)
