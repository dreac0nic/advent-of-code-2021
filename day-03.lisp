;;;; day-03.lisp

(in-package #:day-03)

(defvar *example-input* '("00100"
                          "11110"
                          "10110"
                          "10111"
                          "10101"
                          "01111"
                          "00111"
                          "11100"
                          "10000"
                          "11001"
                          "00010"
                          "01010"))

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (split "\\n" (get-puzzle-input 3)))


;;; Part One
(defun tilt-reading-values (readings)
  "Calculates each bit's tilt based on the readings provided"
  (loop with bits = (make-array (length (car readings)))
        for word in readings
        do (loop for bit across word
                 for index from 0
                 do (incf (aref bits index)
                          (if (char-equal bit #\1) 1 -1)))
        finally (return bits)))


(defun calculate-gamma-epsilon (readings)
  "Calculate the product of the gamma and epsilon of the submarine readings"
  (multiple-value-bind (gamma bit-width)
      (parse-integer (coerce (loop for flag across (tilt-reading-values readings)
                                   collect (if (> flag 0) #\1 #\0))
                             'string)
                     :radix 2)
    (* gamma (logxor gamma (parse-integer (coerce (loop repeat bit-width collect #\1)
                                                  'string)
                                          :radix 2)))))


;; Example
;; (calculate-gamma-epsilon *example-input*)

;; Solution
;; (calculate-gamma-epsilon *puzzle-input*)
;; (submit-answer 3 (calculate-gamma-epsilon *puzzle-input*))


;;; Part Two
(defmacro rating-tester (reading-input predicate-expression)
  "Creates a test fixture for finding an input based on a predicate and producing the value"
  `(parse-integer (loop with queue = ,reading-input
                        while (cdr queue)
                        for bit from 0 upto (length (car queue))
                        for mask = ,predicate-expression
                        do (setf queue
                                 (remove-if (lambda (reading) (not (char= (aref reading bit) mask)))
                                            queue))
                        finally (return (car queue)))
                  :radix 2))


(defun calculate-oxygen-generator-rating (readings)
  "Takes a series of readings and calculates the current oxygen generator performance rating"
  (rating-tester readings
                 (if (>= (aref (tilt-reading-values queue) bit) 0)
                     #\1
                     #\0)))


(defun calculate-co2-scrubber-rating (readings)
  "Takes a series of readings and calculates the current co2 scrubber performance rating"
  (rating-tester readings
                 (if (>= (aref (tilt-reading-values queue) bit) 0)
                     #\0
                     #\1)))


(defun calculate-co2-oxy-product (readings)
  "Takes a series of readings and calculates the product of the co2 scrubber and oxygen generator ratings"
  (* (calculate-oxygen-generator-rating readings)
     (calculate-co2-scrubber-rating readings)))


;; (calculate-co2-oxy-product *puzzle-input*)
;; (submit-answer 3 (calculate-co2-oxy-product *puzzle-input*) :part-two)
