;;;; day-08.lisp

(in-package #:day-08)

(defvar *example-input* "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(defvar *long-example-input* "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (get-puzzle-input 8))

;; Parsing input
(defun generate-signals (input)
  "Takes a string of recorded inputs and generates signal lists"
  (let ((io-split (split " \\| " input)))
    (values (mapcar (lambda (str) (coerce str 'list))
                    (split " " (first io-split)))
            (mapcar (lambda (str) (coerce str 'list))
                    (split " " (second io-split))))))


;;; Part One
(defun count-simple-patterns (input)
  "Counts all the patterns that can be uniquely identified by their length"
  (loop for input-line in (split "\\n" input)
        sum (multiple-value-bind (patterns output)
                (generate-signals input-line)
              (declare (ignore patterns))
              (loop for signal-output in output
                    if (or (= (length signal-output) 2)
                           (= (length signal-output) 4)
                           (= (length signal-output) 3)
                           (= (length signal-output) 7))
                      sum 1))))


;;; Part Two

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg

(defun set= (&rest sets)
  "Tests if two sets contain the same members"
  (let ((prime-set (loop with prime-set = nil
                         for set in sets
                         do (setf prime-set (union prime-set set))
                         finally (return prime-set))))
    (every (lambda (set) (= (length (set-difference prime-set set)) 0))
           sets)))


(defun find-base-patterns (patterns)
  "Takes a list of 7-segment patterns and returns a mask mapping for the basic patterns"
  (loop with masks = nil
        for pattern in patterns
        do (cond
             ((= (length pattern) 2)
              (setf masks (acons 1 pattern masks)))

             ((= (length pattern) 4)
              (setf masks (acons 4 pattern masks)))

             ((= (length pattern) 3)
              (setf masks (acons 7 pattern masks)))

             ((= (length pattern) 7)
              (setf masks (acons 8 pattern masks))))
        finally (return masks)))


(defun map-advanced-patterns (patterns masks)
  "Takes a list of patterns and a seeded pattern mask, adding the advanced patterns to the mask"
  (loop for pattern in patterns
        if (= (length pattern) 5)
          do (cond ((subsetp (cdr (assoc 1 masks)) pattern)
                    (setf masks (acons 3 pattern masks)))

                   ((subsetp (set-difference (cdr (assoc 4 masks))
                                             (cdr (assoc 1 masks)))
                             pattern)
                    (setf masks (acons 5 pattern masks)))
                   (t (setf masks (acons 2 pattern masks))))
        if (= (length pattern) 6)
          do (cond ((subsetp (cdr (assoc 4 masks)) pattern)
                    (setf masks (acons 9 pattern masks)))

                   ((subsetp (set-difference (cdr (assoc 4 masks))
                                             (cdr (assoc 1 masks)))
                             pattern)
                    (setf masks (acons 6 pattern masks)))

                   (t (setf masks (acons 0 pattern masks))))
        finally (return masks)))


(defun decode-signals (signals masks)
  "Takes a set of signals and a completed mask and translates the signals to numbers"
  (loop for power from (1- (length signals)) downto 0
        for digit in signals
        sum (* (expt 10 power)
               (car (rassoc digit masks :test #'set=)))))


(defun sum-signal-outputs (input)
  "Takes a series of notated inputs, translates them, and sums the values of the output signals"
  (loop for input-line in (split "\\n" input)
        sum (multiple-value-bind (patterns outputs)
                (generate-signals input-line)
              (->> patterns
                   find-base-patterns
                   (map-advanced-patterns patterns)
                   (decode-signals outputs)))))
