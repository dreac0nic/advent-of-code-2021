;;;; day-01.lisp

(in-package #:day-01)

(defvar *example-input* '(199 200 208 210 200 207 240 269 260 263))
(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (mapcar #'parse-integer (split "\\n" (get-days-puzzle-input 1))))

;;;; Part One
(defun count-increased-depths (depths)
  "Takes a list of depths and counts how many of them increase from the previous"
  (loop for depth on depths
        if (and (second depth)
                (< (first depth) (second depth)))
          sum 1))


;; Example
;; (count-increased-depths *example-input*)

;; Solution
;; (count-increased-depths *puzzle-input*)
;; (submit-answer 1 (count-increased-depths *puzzle-input*))


;;;; Part Two
(defun count-increased-depths-sliding (depths)
  "Takes a list of depths, and finds how many times a sliding window of 3 depths increases from the last trio"
  (count-increased-depths
   (loop for depth on depths
         if (and (third depth))
           collect (+ (first depth) (second depth) (third depth)))))


;; Example
;; (count-increased-depths-sliding *example-input*)

;; Solution
;; (count-increased-depths-sliding *puzzle-input*)
;; (submit-answer 1 (count-increased-depths-sliding *puzzle-input*) :part-two)
