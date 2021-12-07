;;;; day-07.lisp

(in-package #:day-07)

(defvar *example-input* "16,1,2,0,4,2,7,1,2,14")
(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (get-puzzle-input 7))

;;; Parsing input
(defun generate-crab-pool (input)
  "Creates a pool of crabs"
  (let ((crabs (mapcar #'parse-integer (split "," input))))
    (make-array (length crabs) :initial-contents crabs)))


;;;; Part One
(defun find-linear-fuel-solution (pool-input)
  "Attempts to find the simplest solution for crab motion"
  (loop with pool = (generate-crab-pool pool-input)
        with target-position = (aref (sort pool #'<) (round (/ (length pool) 2)))
        for crab across pool
        sum (abs (- crab target-position))))


;;;; Part Two
(defun find-dynamic-fuel-solution (pool-input)
  "Attempts to find the simplest non-linear solution"
  (loop with pool = (generate-crab-pool pool-input)
        with target-position = (round (/ (loop for crab across pool sum crab) (length pool)))
        for crab across pool
        for offset = (abs (- crab target-position))
        sum (round (/ (* offset (+ offset 1)) 2))))
