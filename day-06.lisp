;;;; day-06.lisp

(in-package #:day-06)

(defvar *example-input* "3,4,3,1,2")

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (get-puzzle-input 6))

;;; Parsing input
(defun spawn-lanternfish (fish-input)
  "Takes a list of numbers representing lanternfish initial states, and creates a state pool for the agents"
  (loop with pool = (make-array 9)
        for fish in (split "," fish-input)
        do (incf (aref pool (parse-integer fish)))
        finally (return pool)))


;;; Part One
(defun count-fish (pool)
  "Takes a pool and counts the number of fishes currently present"
  (loop for fishes across pool
        sum fishes))


(defun tick (pool)
  "Takes a pool of lanternfish state automata and runs a single simulation tick on each"
  (loop with pool-delta = (make-array 9)
        for state from 8 downto 0
        for fishes = (aref pool state)
        if (= state 0)
          do (incf (aref pool-delta 8) fishes)
             (incf (aref pool-delta 6) fishes)
        else
          do (incf (aref pool-delta (1- state)) fishes)
        finally (return pool-delta)))


(defun run-simulation (pool-input &optional (days 80))
  "Takes a number of days (optional) and runs a lanternfish simulation across the provided input"
  (loop with pool = (spawn-lanternfish pool-input)
        repeat days
        do (setf pool (tick pool))
        finally (return (count-fish pool))))


;; Example
;; (run-simulation *example-input*)

;; Solution
;; (run-simulation *puzzle-input*)
;; (submit-answer 6 (run-simulation *puzzle-input*))


;;; Part Two
;; lol forethought


;; Example
;; (run-simulation *example-input* 256) ; => 26984457539
;;                                           26984457539

;; Solution
;; (run-simulation *puzzle-input* 256)
;; (submit-answer 6 (run-simulation *puzzle-input* 256) :part-two)
