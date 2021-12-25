;;;; day-11.lisp

(in-package #:day-11)


(defparameter *example-input* "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defparameter *example-input-small* "11111
19991
19191
19991
11111")

(defparameter *puzzle-input* nil)


;;; Parsing Input
(defun pool-dumbos (input)
  "Takes a text-aligned grid and transforms it into an array of dumbo octopus levels"
  (let ((content (->> (split "\\n" input)
                      (mapcar (lambda (row)
                                (mapcar #'digit-char-p
                                        (coerce row 'list)))))))
    (make-array (list (length content)
                      (length (car content)))
                :initial-contents content)))


;;; Part One
(defun incf-dumbos (pool &optional (amount 1))
  "Increments every dumbo's energy level by a given amount (default 1)"
  (loop for row below (array-dimension pool 0)
        append (loop for col below (array-dimension pool 1)
                     do (incf (aref pool row col) amount)
                     if (> (aref pool row col) 9)
                       collect (cons row col))))


(defun flash (pool row col)
  "Flashes a dumbo at the given coordinates, incrementing each neighbor and returning a list of new coordinates to flash (NOTE: this DOES NOT reset the targeted dumbo)"
  (loop for neigh-row from (max 0 (1- row)) to (min (1- (array-dimension pool 0)) (1+ row))
        append (loop for neigh-col from (max 0 (1- col)) to (min (1- (array-dimension pool 1)) (1+ col))
                     if (not (and (= row neigh-row)
                                  (= col neigh-col)))
                       do (incf (aref pool neigh-row neigh-col))
                     if (> (aref pool neigh-row neigh-col) 9)
                       collect (cons neigh-row neigh-col))))


(defun flash-step (pool)
  "Takes a pool of dumbos and simulates a single charge-and-flash step"
  (let ((flashed-dumbos (loop with flashed = nil
                              with flashing = (incf-dumbos pool)
                              while (car flashing)
                              for row = (caar flashing)
                              for col = (cdar flashing)
                              do (push (pop flashing) flashed)
                                 (loop for possible-dumbo in (flash pool row col)
                                       if (not (or (member possible-dumbo flashing :test #'equal)
                                                   (member possible-dumbo flashed :test #'equal)))
                                         do (setf flashing
                                                  (append1 flashing possible-dumbo))) ; XXX: technically order shouldn't matter ...
                              finally (return flashed))))
    (loop for (row . col) in flashed-dumbos
          do (setf (aref pool row col) 0))
    (values (length flashed-dumbos) flashed-dumbos)))


(defun flash-count (input &optional (steps 100))
  "Performs a number of steps (DEFAULT 100) and counts the number of flashes witness"
  (loop with pool = (pool-dumbos input)
        repeat steps
        sum (flash-step pool)))


;;; Part Two
(defun pool-every (pred pool)
  "Tests every dumbo in the pool with the predicate, returning nil if any fail"
  (loop for row below (array-dimension pool 0)
        always (loop for col below (array-dimension pool 1)
                     always (funcall pred (aref pool row col)))))


(defun find-the-big-flash (input)
  "Searches for a step in which all the dumbos flash"
  (loop with pool = (pool-dumbos input)
        for step from 0
        while (not (pool-every #'zerop pool))
        do (flash-step pool)
        finally (return step)))
