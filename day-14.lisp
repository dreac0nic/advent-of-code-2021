;;;; day-14.lisp

(in-package #:day-14)


(defparameter *example-input* "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defparameter *puzzle-input* nil)


;;; Parsing Input
(defun consume-polymerization (input)
  "Takes a polymerization input and parses the template and the insertion rules"
  (let ((inputs (split "\\n\\n" input))
        insertion-rules)
    (values (coerce (first inputs) 'list)
            (reverse (do-register-groups (pair result)
                         ("([A-Z]{2}) -> ([A-Z])" (second inputs) insertion-rules)
                       (push (cons (->> (coerce pair 'list)
                                        (mapcar (rcurry #'coerce 'character))
                                        (apply #'cons))
                                   (coerce result 'character))
                             insertion-rules))))))


;;; Part One
(defun expand-poly-step (template instructions)
  "Takes a template and instructions, expanding the template via the instructions"
  (loop for (first second) on template
        for matched-rule = (cdr (assoc (cons first second) instructions :test #'equal))
        collect first
        if matched-rule
          collect matched-rule))


(defun count-template-elements (template)
  "Counts the unique elements in a template"
  (loop with counts = nil
        for element in template
        for element-count = (assoc element counts)
        if element-count
          do (incf (cdr (assoc element counts)))
        else
          do (setf counts (acons element 1 counts))
        finally (return (sort counts #'> :key #'cdr))))


(defun rank-template (template)
  "Rank the template based on the subtraction of the highest and lowest members"
  (let ((counts (count-template-elements template)))
    (- (cdr (first counts))
       (cdar (last counts)))))


(defun perform-poly-rank (input &key (times 10))
  "Performs a poly sequence, then ranks that sequence upon 10 iterations"
  (multiple-value-bind (template instructions)
      (consume-polymerization input)
    (rank-template (loop with ex-template = template
                         for step from 1 to times
                         do (setf ex-template (expand-poly-step ex-template instructions))
                         finally (return ex-template)))))


;;; Part Two
(defun synthesize-first-pairs (template)
  "Returns a seeding first copy of the template in the pairs format"
  (loop with pairs = nil
        for (left right) on template
        for pair = (cons left right)
        if (assoc pair pairs :test #'equal)
          do (incf (cdr (assoc pair pairs :test #'equal)))
        else
          do (setf pairs (acons (cons left right) 1 pairs))
        finally (return pairs)))


(defun synthesize-step (pairs instructions)
  "Synthesizes a new count of pairs for a single step"
  (loop with pairs-prime = nil
        for (pair . count) in pairs
        for left = (cons (car pair) (cdr (assoc pair instructions :test #'equal)))
        for right = (cons (cdr (assoc pair instructions :test #'equal)) (cdr pair))

        ;; sum left pair
        if (assoc left pairs-prime :test #'equal)
          do (incf (cdr (assoc left pairs-prime :test #'equal)) count)
        else
          do (setf pairs-prime (acons left count pairs-prime))

             ;; sum right pair
        if (assoc right pairs-prime :test #'equal)
          do (incf (cdr (assoc right pairs-prime :test #'equal)) count)
        else
          do (setf pairs-prime (acons right count pairs-prime))
        finally (return pairs-prime)))


(defun count-pair-elements (pairs)
  "Counts the unique first elements of all pairs"
  (loop with counts = nil
        for (pair . count) in pairs
        for glyph = (car pair)
        if glyph
          do (if (assoc glyph counts)
                 (incf (cdr (assoc glyph counts)) count)
                 (setf counts (acons glyph count counts)))
        finally (return (sort counts #'> :key #'cdr))))


(defun rank-pairs (pairs)
  "Ranks the polymere pairs based on frequency"
  (let ((counts (count-pair-elements pairs)))
    (- (cdr (first counts))
       (cdar (last counts)))))


(defun perform-deep-poly-rank (input &key (times 40))
  "Counts the pairs available in a poly sequence, ranking the finaly sequence based on the common elements"
  (multiple-value-bind (template instructions)
      (consume-polymerization input)
    (rank-pairs (loop repeat (1+ times)
                         for pairs = (synthesize-first-pairs template)
                           then (synthesize-step pairs instructions)
                         finally (return pairs)))))
