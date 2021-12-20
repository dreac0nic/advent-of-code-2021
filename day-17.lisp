;;;; day-17.lisp

(in-package #:day-17)


(defvar *puzzle-input* nil)

;;; Parsing Input
(defun parse-target-boundary (input)
  "Reads an input string and takes an x/y bound from the input"
  (let ((ranges (mapcar (lambda (range-input)
                          (-<>> (subseq range-input 2)
                                (split "\\.\\.")
                                (mapcar #'parse-integer)
                                (sort <> #'<)
                                (apply #'cons)))
                        (split ", " (second (split ": " input))))))
    (list (cons :x (first ranges))
          (cons :y (second ranges)))))

;;; Part One
(defun within-range-p (value range)
  "Tests if the value is within the given range"
  (and (> value (1- (car range)))
       (< value (1+ (cdr range)))))


;; (defun trace-launch (vx vy target-boundary)
;;   "Tests if the given velocity will hit the target boundary"
;;   (loop for dx = vx then (- dx (signum dx))
;;         for dy = vy then (1- dy)
;;         for x = 0 then (+ x dx)
;;         for y = 0 then (+ y dy)
;;         collect (cons x y) into trace
;;         if (and (within-range-p x (cdr (assoc :x target-boundary)))
;;                 (within-range-p y (cdr (assoc :y target-boundary))))
;;           return trace
;;         else if (or (> x (cdr (cdr (assoc :x target-boundary))))
;;                     (< y (car (cdr (assoc :y target-boundary)))))))


;; (defun find-maximum-launch-height (target-input)
;;   "Parses the target input and attempts to find the launch y-velocity that will reach the highest peak"
;;   (loop with target-boundary = (parse-target-boundary target-input)
;;         with x-range = (cdr (assoc :x target-boundary))
;;         for vx from 1 to (cdr x-range)
;;         do (loop with y-range = (cdr (assoc :y target-boundary))
;;                  for vy from (- (abs (car y-range))) to (abs (car y-range))
;;                  do (format t "~&~Dx~D~%" vx vy))))

;;; Part Two
