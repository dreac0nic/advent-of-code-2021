;;;; utility.lisp

(in-package #:utility)


(defun single? (list)
  "Tests whether a list contains only a single element"
  (and (consp list) (null (cdr list))))


(defun append1 (list object)
  "Appends a single element to the end of a list"
  (append list (list object)))


(defun map-int (fn num)
  "Accumulates up to a number, cons a list of the result of that current accumulator value with the function"
  (let ((accumulator nil))
    (dotimes (index num)
      (push (funcall fn index) accumulator))
    (nreverse accumulator)))


(defun filter (pred list)
  "Returns a new list of only the values that match the predicate"
  (let ((accumulator nil))
    (dolist (item list)
      (let ((val (funcall pred item)))
        (when val (push val accumulator))))
    (nreverse accumulator)))


(defun most (fn list)
  "Scores each element of the list via a given function, then returns the winning element of the list"
  (if (null list)
      (values nil nil)
      (let* ((wins (car list))
             (max (funcall fn wins)))
        (dolist (object (cdr list))
          (let ((score (funcall fn object)))
            (when (> score max)
              (setf wins object
                    max score))))
        (values wins max))))


(defun compose (&rest fns)
  "Combines a list of functions into a single function of the result of one combined into the next"
  (destructuring-bind (fn1 . rest)
      (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (v f) (funcall f v))
              rest
              :initial-value (apply fn1 args)))))


(defun disjoin (fn &rest fns)
  "Returns a function that gives the union of its function's values"
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        (lambda (&rest args)
          (or (apply fn args) (apply disj args))))))


(defun conjoin (fn &rest fns)
  "Returns a function that gives the intersection of its function's values"
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        (lambda (&rest args)
          (and (apply fn args) (apply disj args))))))


(defun curry (fn &rest args)
  "Returns a function curried with the given values"
  (lambda (&rest args2)
    (apply fn (append args args2))))


(defun curry (fn &rest args)
  "Returns a function curried with the given values"
  (lambda (&rest args2)
    (apply fn (append args args2))))


(defun rcurry (fn &rest args)
  "Returns a function curried with the given values, as the right-most parameters"
  (lambda (&rest args2)
    (apply fn (append args2 args))))


(defun always (x)
  "Always returns X"
  (lambda (&rest args) x))


(defun clamp (val floor ceiling)
  "Clamps the value at a minimum of the floor and a maximum of the ceiling"
  (min ceiling (max val floor)))
