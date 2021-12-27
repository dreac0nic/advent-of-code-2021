;;;; day-12.lisp

(in-package #:day-12)


(defparameter *example-input* "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defparameter *example-input-long* "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(defparameter *example-input-huge* "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(defparameter *puzzle-input* nil)


;;; Parsing Input
(defun small-cave? (cave-keyword)
  "Predicate to test if a cave is small"
  (every #'lower-case-p (string cave-keyword)))


(defun big-cave? (cave-keyword)
  "Predicate to test if a cave is big"
  (every #'upper-case-p (string cave-keyword)))


(defun parse-edges (input)
  "Builds a list of edges"
  (mapcar (lambda (edge)
            (->> (split "\\-" edge)
                 (mapcar (rcurry #'intern :keyword))
                 (apply #'cons)))
          (split "\\n" input)))


(defun parse-nodes (input)
  "Builds a list of unique nodes"
  (->> (split "\\n" input)
       (mapcar (lambda (edge)
                 (mapcar (rcurry #'intern :keyword)
                         (split "\\-" edge))))
       (apply #'append)
       remove-duplicates
       (mapcar (rcurry #'cons nil))))


(defun parse-caves (input)
  "Takes an input and parses a graph to navigate"
  (loop with paths = (parse-edges input)
        with caves = (parse-nodes input)
        for (to . from) in paths
        do (setf (cdr (assoc to caves))
                 (append1 (cdr (assoc to caves)) from)

                 (cdr (assoc from caves))
                 (append1 (cdr (assoc from caves)) to))
        finally (return caves)))


;;; Part One
(defun find-exit-paths (caves &optional (current-node :|start|) path visited)
  "Attempts to find the exit"
  (let ((possible-exits (loop for exit in (cdr (assoc current-node caves))
                              if (not (member exit visited))
                                collect exit)))
    (cond
      ((eql current-node :|end|) (list (reverse (cons current-node path))))

      ((> (length possible-exits) 0)
       (loop for exit in possible-exits
             for paths = (find-exit-paths caves
                                          exit
                                          (cons current-node path)
                                          (if (small-cave? current-node)
                                              (cons current-node visited)
                                              visited))
             if paths
               append paths))

      (t nil))))


(defun count-cave-paths (input)
  "Counts the number of available paths through the caves"
  (length (find-exit-paths (parse-caves input))))


;;; Part Two
(defun path-completed? (cave-path)
  "Tests if a path is completed"
  (eql (car cave-path) :|end|))


(defun count-small-cave-duplicates (path)
  "Counts the number of small caves duplicated in the path"
  (let ((small-caves (remove-if-not #'small-cave? path)))
    (- (length small-caves)
       (length (remove-duplicates small-caves)))))


(defun count-big-brain-paths (input &optional caves paths)
  "Attempt to count paths using permutation and advanced pathing (2x small visitation)"
  (cond
    ((null input) (error "No input given, idiot"))

    ((null caves)
     (big-brain input (parse-caves input) paths))

    ((null paths)
     (big-brain input caves (list (list :|start|))))

    ((every #'path-completed? paths)
     (length (mapcar #'reverse paths))) ; lol reverse

    (t (big-brain input
                  caves
                  (loop for path in paths
                        for current-cave = (car path)
                        for duplicates = (count-small-cave-duplicates path)
                        append (if (path-completed? path)
                                   (list path)
                                   (loop for exit in (cdr (assoc current-cave caves))
                                         if (and (not (eql exit :|start|))
                                                 (or (big-cave? exit)
                                                     (zerop duplicates)
                                                     (not (member exit path))))
                                           collect (cons exit path))))))))
