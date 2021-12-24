;;;; day-17.lisp

(in-package #:day-18)


(defvar *example-input* "[1,1]
[2,2]
[3,3]
[4,4]")


(defvar *example-input-long* "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]")

(defvar *example-input-full* "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(defvar *puzzle-input* nil)

;;; Parsing Input
(defstruct (snail (:conc-name s-)
                  (:print-function print-snail))
  (left 0)
  (right 0))


(defun print-snail (snail stream depth)
  "Prints a non-readablereadable snail form"
  (declare (ignore depth))
  (format stream "[~A,~A]" (s-left snail) (s-right snail)))


(defun read-snail (stream char)
  "Reads a snail form into lisp objects"
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\ ) ; treat commas like whitespace
    (set-syntax-from-char #\] #\)) ; treat brackets as terminating characters

    (let ((children (read-delimited-list #\] stream t)))
      (if (>= (length children) 2)
          `(make-snail :left ,(first children)
                       :right ,(second children))
          (error "Snail number requires 2 children in a pair (found: ~D): ~S in ~S" (length children) children stream)))))

;; (set-macro-character #\[ #'read-snail)


(defun consume-snails (input)
  "Takes a string of line-seperated snail numbers and returns a list of ingested snail numbers"
  ;; XXX: ... yeah
  (mapcar (compose #'eval #'read-from-string) (split "\\n" input)))

;;; Part One
(defun regular? (snail)
  "Tests if the snail element is a regular number or a pair"
  (numberp snail))


(defun pair? (snail)
  "Tests if the snail element is a pair of snail numbers"
  (snail-p snail))


(defun add-side (operand snail side)
  "Recursively tries to add the operand to the value closest to the side given"
  (if (numberp snail)
      (+ snail operand)
      (apply #'make-snail
             (append (list side
                           (add-side operand
                                     (funcall (case side
                                                (:left #'s-left)
                                                (:right #'s-right))
                                              snail)
                                     side))
                     (if (eql side :left)
                         (list :right (s-right snail))
                         (list :left (s-left snail)))))))


(defun snail-explode (snail &optional (depth 0))
  "Attempts to explode a snail number, if said number needs exploding"
  (cond
    ((regular? snail) snail)

    ((>= depth 4)
     (values 0 t (s-left snail) (s-right snail)))

    (t
     (multiple-value-bind (snail-value exploding? left-tree right-tree)
         (snail-explode (s-left snail) (1+ depth))
       (if exploding?
           (if right-tree
               (values (make-snail :left snail-value
                                   :right (add-side right-tree (s-right snail) :left))
                       t
                       left-tree
                       nil)
               (values (make-snail :left snail-value
                                   :right (s-right snail))
                       t
                       left-tree
                       right-tree))
           (multiple-value-bind (snail-value exploding? left-tree right-tree)
               (snail-explode (s-right snail) (1+ depth))
             (if exploding?
                 (if left-tree
                     (values (make-snail :left (add-side left-tree (s-left snail) :right)
                                         :right snail-value)
                             t
                             nil
                             right-tree)
                     (values (make-snail :left (s-left snail)
                                         :right snail-value)
                             t
                             left-tree
                             right-tree))
                 (make-snail :left (s-left snail)
                             :right (s-right snail)))))))))


(defun snail-split (snail)
  "Attempts to split a snail number, if necessary"
  (cond
    ((and (numberp snail)
          (< snail 10))
     snail)

    ((numberp snail)
     (values (make-snail :left (floor (/ snail 2))
                         :right (ceiling (/ snail 2)))
             t))

    (t
     (multiple-value-bind (snail-value splitting?)
         (snail-split (s-left snail))
       (if splitting?
           (values (make-snail :left snail-value
                               :right (s-right snail))
                   t)
           (multiple-value-bind (snail-value splitting?)
               (snail-split (s-right snail))
             (if splitting?
                 (values (make-snail :left (s-left snail)
                                     :right snail-value)
                         t)
                 (make-snail :left (s-left snail)
                             :right (s-right snail)))))))))


(defun snail-reduce (snail)
  "Takes a snail number and reduces it to its simplest form"
  (loop with num = snail
        with reduced? = nil
        while (not reduced?)
        do (multiple-value-bind (snail-delta exploding?)
               (snail-explode num)
             (if exploding?
                 (setf num snail-delta)
                 (multiple-value-bind (snail-delta splitting?)
                     (snail-split num)
                   (if splitting?
                       (setf num snail-delta)
                       (setf reduced? t)))))
        finally (return num)))


(defun snail-+ (&rest operands)
  "Adds two snail numbers together"
  (reduce (lambda (left right)
            (snail-reduce (make-snail :left left
                                      :right right)))
          operands))


(defun snail-magnitude (snail)
  "Calculates a snail number's magnitude"
  (if (numberp snail)
      snail
      (+ (* 3 (snail-magnitude (s-left snail)))
         (* 2 (snail-magnitude (s-right snail))))))


(defun find-magnitude-of-homework (homework-input)
  "Mom do I have to?"
  (->> (consume-snails homework-input)
       (apply #'snail-+)
       snail-magnitude))

;;; Part Two
(defun find-largest-pair-of-homeworks (homework-input)
  "Mr. Belladair says if we do the extra credit we'll get a pizza party"
  (loop with homework = (consume-snails homework-input)
        for left in homework
        maximize (loop for right in homework
                       if (not (equal left right))
                         maximize (snail-magnitude (snail-+ left right)))))
