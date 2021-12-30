;;;; day-13.lisp

(in-package #:day-13)


(defparameter *example-input* "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defparameter *puzzle-input* nil)


;;; Parsing Input
(defun parse-coordinates (input)
  "Takes a string of coordinates and applies them to an array"
  (loop with coords = (mapcar (lambda (coord)
                                 (->> (split "," coord)
                                      (mapcar #'parse-integer)
                                      (apply #'cons)))
                              (split "\\n" input))
        with max-x = -1
        with max-y = -1
        for (x . y) in coords
        if (> x max-x)
          do (setf max-x x)
        if (> y max-y)
          do (setf max-y y)
        finally (return (loop with marks = (make-array (list (1+ max-y) (1+ max-x))
                                                       :initial-element nil)
                              for (x . y) in coords
                              do (setf (aref marks y x) t)
                              finally (return marks)))))


(defun parse-folds (input)
  "Takes a string of paper folds"
  (reverse (let (motions)
             (do-register-groups (((compose (rcurry #'intern :keyword) #'string-upcase) orientation) (#'parse-integer divisor))
                 ("fold along (x|y)=(\\d+)" input motions)
               (push (cons orientation divisor) motions)))))


(defun parse-thermal-instructions (input)
  (let* ((inputs (split "\\n\\n" input))
         (marks (parse-coordinates (first inputs)))
         (folds (parse-folds (second inputs))))
    (values marks folds)))


(defun print-sheet (sheet)
  "Takes a marked sheet and prints it in the format used in the explanation"
  (loop for row below (array-dimension sheet 0)
        do (loop for col below (array-dimension sheet 1)
                 do (princ (if (aref sheet row col)
                               #\#
                               #\.)))
           (format t "~%")))


;;; Part One
(defun copy-sheet (sheet &key rows cols)
  "Copies an array"
  (loop with sheet-prime = (make-array (list (or rows
                                                 (array-dimension sheet 0))
                                             (or cols
                                                 (array-dimension sheet 1))))
        for row below (array-dimension sheet-prime 0)
        do (loop for col below (array-dimension sheet-prime 1)
                 do (setf (aref sheet-prime row col)
                          (aref sheet row col)))
        finally (return sheet-prime)))


(defun fold-y (sheet divider)
  "Folds a sheet on itself, horizontal, from the dividing line"
  (when (or (< divider 0)
            (>= divider (array-dimension sheet 0)))
    (error "Bad coordinate ~D, should be within 0 to ~D" divider (1- (array-dimension sheet 1))))
  (loop with folded = (copy-sheet sheet :rows divider)
        for row from (1+ divider) below (array-dimension sheet 0)
        for row-prime from (1- divider) downto 0
        do (loop for col from 0 below (array-dimension folded 1)
                 if (and (>= row-prime 0)
                         (< row-prime (array-dimension folded 0)))
                   do (setf (aref folded row-prime col)
                            (or (aref folded row-prime col)
                                (aref sheet row col))))
        finally (return folded)))


(defun fold-x (sheet divider)
  "Folds a sheet on itself, vertically, from the dividing line"
  (when (or (< divider 0)
            (>= divider (array-dimension sheet 0)))
    (error "Bad coordinate ~D, should be within 0 to ~D" (1- (array-dimension sheet 0)) divider))
  (loop with folded = (copy-sheet sheet :cols divider)
        for row below (array-dimension folded 0)
        do (loop for col from (1+ divider) below (array-dimension sheet 1)
                 for col-prime from (1- divider) downto 0
                 if (and (>= col-prime 0)
                         (< col-prime (array-dimension folded 1)))
                   do (setf (aref folded row col-prime)
                            (or (aref folded row col-prime)
                                (aref sheet row col))))
        finally (return folded)))


(defun count-first-fold-dots (input)
  "Takes a page of dots and fold instructions, then counts the number of resulting dots after only one fold"
  (multiple-value-bind (sheet instructions)
      (parse-thermal-instructions input)
    (loop with final-sheet = (funcall (case (caar instructions)
                                        (:X #'fold-x)
                                        (:Y #'fold-y))
                                      sheet
                                      (cdar instructions))
          for row below (array-dimension final-sheet 0)
          sum (loop for col below (array-dimension final-sheet 1)
                    if (aref final-sheet row col)
                      sum 1))))

;;; Part Two
(defun fold-dots (input)
  "Takes a page of dots and fold instructions, then counts the number of resulting dots"
  (print-sheet
   (multiple-value-bind (sheet instructions)
       (parse-thermal-instructions input)
     (loop for (orientation . degree) in instructions
           do (setf sheet
                    (funcall (case orientation
                               (:X #'fold-x)
                               (:Y #'fold-y))
                             sheet
                             degree))
           finally (return sheet)))))
