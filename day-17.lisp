;;;; day-17.lisp

(in-package #:day-17)


(defvar *example-input* "target area: x=20..30, y=-10..-5")

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
(defun within-range? (value range)
  "Tests if the value is within the given range"
  (and (> value (1- (car range)))
       (< value (1+ (cdr range)))))


(defun within-boundary? (x y boundary)
  "Tests if the x/y coordinate is within the boundary"
  (and (within-range? x (cdr (assoc :x boundary)))
       (within-range? y (cdr (assoc :y boundary)))))


(defun trace-launch (vx vy target-boundary)
  "Tests if the given velocity will hit the target boundary"
  (loop for x = 0 then (+ x dx)
        for y = 0 then (+ y dy)
        for dx = vx then (- dx (signum dx))
        for dy = vy then (1- dy)
        collect (cons x y) into trace
        if (within-boundary? x y target-boundary)
          return trace
        else if (or (> x (cdr (cdr (assoc :x target-boundary))))
                    (< y (car (cdr (assoc :y target-boundary)))))
               return nil))


(defun find-maximum-launch-height (target-input)
  "Parses the target input and attempts to find the launch y-velocity that will reach the highest peak"
  (loop with target-boundary = (parse-target-boundary target-input)
        with x-range = (cdr (assoc :x target-boundary))
        for vx from 1 to (cdr x-range) ; TODO: find minimum x?
        maximize (loop with y-range = (cdr (assoc :y target-boundary))
                       for vy from (- (abs (car y-range))) to (abs (car y-range))
                       maximize (loop for coord in (trace-launch vx vy target-boundary)
                                      maximize (cdr coord)))))

;;; Part Two
(defun count-valid-velocities (target-input)
  "Parses the target input and attempts to find the number of valid launch velocities"
  (loop with target-boundary = (parse-target-boundary target-input)
        with x-range = (cdr (assoc :x target-boundary))
        for vx from 1 to (cdr x-range) ; TODO: find minimum x?
        sum (loop with y-range = (cdr (assoc :y target-boundary))
                  for vy from (- (abs (car y-range))) to (abs (car y-range))
                  if (trace-launch vx vy target-boundary)
                    sum 1)))
