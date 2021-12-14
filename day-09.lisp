;;;; day-09.lisp

(in-package #:day-09)

(defvar *example-input* "2199943210
3987894921
9856789892
8767896789
9899965678")
(defvar *puzzle-input* nil)

;;; Parsing Input
(defun generate-field (input)
  "Takes a 2d field of integers as a string and generates an array field that matches"
  (let ((contents (mapcar (lambda (line)
                            (mapcar #'parse-integer
                                    (split "" line)))
                          (split "\\n" input))))
    (make-array (list (length contents) (length (car contents)))
                :initial-contents contents)))

;;; Part One
(defun clamp (val floor ceiling)
  "Clamps the value at a minimum of the floor and a maximum of the ceiling"
  (min ceiling (max val floor)))


(defun neighbors (array row col &optional (stride 1))
  "Finds all neighbors, respecting array boundaries"
  (loop for offset from (- stride) to stride
        for row-prime = (clamp (+ row offset) 0 (1- (array-dimension array 0)))
        for col-prime = (clamp (+ col offset) 0 (1- (array-dimension array 1)))
        if (not (= row row-prime))
          collect (aref array row-prime col)
        if (not (= col col-prime))
          collect (aref array row col-prime)))


(defun sum-low-points (input)
  "Takes a field input and produces the sum of all the defined low points"
  (loop with field = (generate-field input)
        for row from 0 below (array-dimension field 0)
        sum (loop for col from 0 below (array-dimension field 1)
                  for cell = (aref field row col)
                  if (< cell (apply #'min (neighbors field row col)))
                    sum (1+ cell))))

;;; Part Two
(defun neighbors-coords (array row col &optional (stride 1))
  "Finds all neighbors, respecting array boundaries"
  (loop for offset from (- stride) to stride
        for row-prime = (clamp (+ row offset) 0 (1- (array-dimension array 0)))
        for col-prime = (clamp (+ col offset) 0 (1- (array-dimension array 1)))
        if (not (= row row-prime))
          collect (cons row-prime col)
        if (not (= col col-prime))
          collect (cons row col-prime)))


(defun collect-low-points (field)
  "Collects a list of low points within the field"
  (apply #'concatenate
         'list
         (loop for row from 0 below (array-dimension field 0)
               collect (loop for col from 0 below (array-dimension field 1)
                             for cell = (aref field row col)
                             if (< cell (apply #'min (neighbors field row col)))
                               collect (cons row col)))))


(defun flood-fill-basin (field row col)
  "Takes a row and column coordinate in a field and flood-fills the area encompsed by 9s, returns the coorindates in the basin"
  (loop repeat 1000
        with basin = nil
        with visited = nil
        with visit-queue = `((,row . ,col))
        while (car visit-queue)
        for coord = (car visit-queue)
        for cell = (aref field (car coord) (cdr coord))
        do (when (not (member coord visited :test #'equal))
             (push coord visited))
           (setf visit-queue
                 (concatenate 'list
                              (cdr visit-queue)
                              (loop for neighbor in (neighbors-coords field (car coord) (cdr coord))
                                    for neighbor-cell = (aref field (car neighbor) (cdr neighbor))
                                    if (and (< cell neighbor-cell)
                                            (not (= neighbor-cell 9)))
                                      collect neighbor)))
        finally (return visited)))


(defun product-of-basins (input)
  "Takes a formatted field input and returns the size product of three largest basins discovered"
  (apply #'*
         (loop repeat 3
               for size in (sort (loop with field = (generate-field input)
                                       for low-point in (collect-low-points field)
                                       collect (length (flood-fill-basin field (car low-point) (cdr low-point))))
                                 #'>)
               collect size)))
