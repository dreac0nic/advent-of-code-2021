;;;; day-10.lisp

(in-package #:day-10)

(defvar *example-input* "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")
(defvar *puzzle-input* nil)

;;; Parsing Input
(defun generate-token-lines (input)
  "Takes a formatted input and translates the lines into tokenized lists"
  (mapcar (lambda (line) (coerce line 'list))
          (split "\\n" input)))

;;; Part One
(defparameter *token-pairs* '((#\( . #\))
                              (#\[ . #\])
                              (#\{ . #\})
                              (#\< . #\>))
  "A list of paired tokens")

(defparameter *syntax-token-error-scores* '((#\) . 3)
                                            (#\] . 57)
                                            (#\} . 1197)
                                            (#\> . 25137))
  "The score of each missing token")


(defun validate-token-pairs (token-input)
  "Attempts to validate a series of token pairs"
  (loop with token-stack = nil
        for token in token-input
        for current-token = (car token-stack)
        if (member token *token-pairs* :key #'car)
          do (push token token-stack)
        if (member token *token-pairs* :key #'cdr)
          do (if (char= token (cdr (assoc current-token *token-pairs*)))
                 (pop token-stack)
                 (return (cdr (assoc token *syntax-token-error-scores*))))
        finally (return (if (not (car token-stack))
                            t
                            (loop for missing-token in token-stack
                                  collect (cdr (assoc missing-token *token-pairs*)))))))


(defun score-incorrect-sequences (input)
  "Validates the navigation subsystems and sums the syntax results"
  (loop for token-line in (generate-token-lines input)
        for validation-result = (validate-token-pairs token-line)
        if (numberp validation-result)
          sum validation-result))

;;; Part Two
(defparameter *token-completion-scores* '((#\) . 1)
                                          (#\] . 2)
                                          (#\} . 3)
                                          (#\> . 4))
  "The score for completed tokens")


(defun calculate-completed-scores (completed-tokens)
  "Takes a list of completed tokens and calculates their score"
  (loop with score = 0
        for token in completed-tokens
        do (setf score (+ (* 5 score)
                          (cdr (assoc token *token-completion-scores*))))
        finally (return score)))


(defun score-completed-sequences (input)
  "Validates the navigation subsystems and sums the completed sequences"
  (let ((scores (sort (loop for token-line in (generate-token-lines input)
                            for validation-result = (validate-token-pairs token-line)
                            if (listp validation-result)
                              collect (calculate-completed-scores validation-result))
                      #'>)))
    (nth (floor (/ (length scores) 2))
         scores)))
