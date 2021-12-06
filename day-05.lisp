;;;; day-05.lisp

(in-package #:day-05)

(defvar *example-input* "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (get-puzzle-input 5))

;;; Parsing input
(defstruct point
  "A 2d point representing a position on an integer grid"
  x y)


(defun generate-lines (line-defs)
  "Takes a string of line definitions, and returns a list of cons-cells containing the base and leg of the line"
  (loop with max-x = -1
        with max-y = -1
        with lines = nil
        for line-input in (split "\\n" line-defs)
        do (setf lines
                 (cons (let ((point-values (mapcar #'parse-integer (split ",| -> " line-input))))
                         (setf max-x (max max-x (car point-values) (caddr point-values))
                               max-y (max max-y (cadr point-values) (cadddr point-values)))
                         (cons (make-point :x (first point-values)
                                           :y (second point-values))
                               (make-point :x (third point-values)
                                           :y (fourth point-values))))
                       lines))
        finally (return (values lines max-x max-y))))


;;; Part One
(defun generate-simple-marked-grid (line-defs)
  "Takes a series of line definitions and generates a marked grid designating overlapping single-dimension, continguous lines"
  (multiple-value-bind (lines cols rows)
      (generate-lines line-defs)
    (format t "~&~a, ~a~%" rows cols)
    (loop with grid = (make-array (list (1+ rows) (1+ cols)) :initial-element 0)
          for line in lines
          for base = (car line)
          for leg = (cdr line)
          if (= (point-x base) (point-x leg))
            do (loop with start = (min (point-y base) (point-y leg))
                     with end = (max (point-y base) (point-y leg))
                     for crawl from start to end
                     do (incf (aref grid crawl (point-x base))))
          if (= (point-y base) (point-y leg))
            do (loop with start = (min (point-x base) (point-x leg))
                     with end = (max (point-x base) (point-x leg))
                     for crawl from start to end
                     do (incf (aref grid (point-y base) crawl)))
          finally (return grid))))


(defun calculate-grid-score (grid)
  "Takes a populated grid and calculates the number of overlaps on the board"
  (loop for row below (first (array-dimensions grid))
        sum (loop for col below (second (array-dimensions grid))
                  if (> (aref grid row col) 1)
                    sum 1)))


;; Example
;; (calculate-grid-score (generate-simple-marked-grid *example-input*))

;; Solution
;; (calculate-grid-score (generate-simple-marked-grid *puzzle-input*))
;; (submit-answer 5 (calculate-grid-score (generate-simple-marked-grid *puzzle-input*)))


;;; Part Two
(defun generate-advanced-marked-grid (line-defs)
  "Takes a seris of line definitions and generates a marked grid designating overlapping contiguous lines."
  (multiple-value-bind (lines cols rows)
      (generate-lines line-defs)
    (loop with grid = (make-array (list (1+ rows) (1+ cols)) :initial-element 0)
          for line in lines
          for base = (car line)
          for leg = (cdr line)
          do (loop for crawl-x = (point-x base) then (+ crawl-x (cond
                                                                  ((> (point-x leg) (point-x base)) 1)
                                                                  ((< (point-x leg) (point-x base)) -1)
                                                                  (t 0)))
                   for crawl-y = (point-y base) then (+ crawl-y (cond
                                                                  ((> (point-y leg) (point-y base)) 1)
                                                                  ((< (point-y leg) (point-y base)) -1)
                                                                  (t 0)))
                   do (incf (aref grid crawl-y crawl-x))
                   while (not (and (= crawl-x (point-x leg))
                                   (= crawl-y (point-y leg)))))
          finally (return grid))))


;; Example
;; (calculate-grid-score (generate-advanced-marked-grid *example-input*))

;; Solution
;; (calculate-grid-score (generate-advanced-marked-grid *puzzle-input*))
;; (submit-answer 5 (calculate-grid-score (generate-advanced-marked-grid *puzzle-input*)) :part-two)
