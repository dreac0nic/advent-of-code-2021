;;;; day-04.lisp

(in-package #:day-04)

(defvar *example-input* "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (get-puzzle-input 4))


(loop for row from 0 below 5
      do (loop for col from 0 below 5
               do (format t "~&~a, ~a~%" row col)))


;;; Parsing input
(defun read-bingo-input (input &key (rows 5) (cols 5))
  "Takes a string input and extracts a bingo configuration"
  (let* ((inputs (split "\\n\\n" input))
         (numbers (mapcar #'parse-integer (split "," (car inputs))))
         (board-inputs (cdr inputs)))
    (values numbers
            (loop for board-def in board-inputs
                  collect (with-input-from-string (in board-def)
                            (loop with board = (make-array (list cols rows))
                                  for row from 0 below rows
                                  do (loop for col from 0 below cols
                                           do (setf (aref board row col)
                                                    (cons (read in) nil)))
                                  finally (return board)))))))


;;; Part One
(defun board-win-p (board)
  "Tests for any valid win conditions present on the given board"
  (loop with col-tests = (make-array (second (array-dimensions board)) :initial-element t)
        for row below (first (array-dimensions board))
        if (loop with row-test = t
                 for col below (second (array-dimensions board))
                 for markedp = (cdr (aref board row col))
                 if (not markedp)
                   do (setf (aref col-tests col) nil
                            row-test nil)
                 finally (return row-test))
          return t
        finally (return (some #'identity col-tests))))


;; Sam's flask code
(coerce (mapcar #'code-char '(115 116
                              101 112
                              032 111
                              110 032
                              109 101))
        'string) ; => "step on me"


(defun calculate-board-score (board)
  "Takes a bingo board and calculates the sum of all unmarked spaces"
  (loop for row below (first (array-dimensions board))
        sum (loop for col below (second (array-dimensions board))
                  for cell = (aref board row col)
                  if (not (cdr cell))
                    sum (car cell))))


(defun run-bingo-game (input)
  "Takes game input and runs the bingo game, returning identifying information about the winning board"
  (multiple-value-bind (numbers boards)
      (read-bingo-input input)
    (loop for bingo in numbers
          for board-result = (loop for board in boards
                                   do (loop for row below (first (array-dimensions board))
                                            do (loop for col below (second (array-dimensions board))
                                                     do (when (= bingo (car (aref board row col)))
                                                          (setf (cdr (aref board row col)) t))))
                                   if (board-win-p board)
                                     return (calculate-board-score board))
          if board-result
            return (* bingo board-result))))


;; Example
;; (run-bingo-game *example-input*)

;; Solution
;; (run-bingo-game *puzzle-input*)
;; (submit-answer 4 (run-bingo-game *puzzle-input*))


;;; Part Two
(defun run-losing-bingo-game (input)
  "Takes game input and runs the bingo game, return identifying information about the LAST winning board"
  (multiple-value-bind (numbers boards)
      (read-bingo-input input)
    (loop with last-completed-board = nil
          for bingo in numbers
          do (loop while (car boards)
                   for board in boards
                   for index from 0
                   do (loop for row below (first (array-dimensions board))
                            do (loop for col below (second (array-dimensions board))
                                     do (when (= bingo (car (aref board row col)))
                                          (setf (cdr (aref board row col)) t))))
                   if (board-win-p board)
                     do (setf last-completed-board board
                              boards (remove board boards)))
          if (not (car boards))
            return (* bingo (calculate-board-score last-completed-board)))))


;; Example
;; (run-losing-bingo-game *example-input*)

;; Solution
;; (run-losing-bingo-game *puzzle-input*)
;; (submit-answer 4 (run-losing-bingo-game *puzzle-input*) :part-two)
