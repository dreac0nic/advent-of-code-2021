;;;; day-15.lisp

(in-package #:day-15)


(defparameter *example-input* "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defparameter *example-input-full* "11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479")

(defparameter *puzzle-input* nil)


;;; Parsing Inputs
(defun carve-cave (input)
  "Takes a string input cave and builds a traversable map from it"
  (let ((cavern (mapcar (compose (curry #'mapcar #'digit-char-p)
                                 (rcurry #'coerce 'list))
                        (split "\\n" input))))
    (make-array (list (length cavern) (length (car cavern)))
                :initial-contents cavern)))


;;; Part One
(defun make-queue (&rest initial-elements)
  "Creates a priority queue"
  (let ((queue (cons nil nil)))
    (loop for (obj priority) on initial-elements by #'cddr
          do (enqueue obj (or priority 0) queue)
          finally (return queue))))


(defun enqueue (obj priority queue)
  "Pushes an item onto the queue"
  (cond
    ;; add the first element
    ((null (car queue))
     (setf (cdr queue) (setf (car queue)
                             (list (cons obj priority)))))

    ;; cons element to the front of the queue
    ((< priority (cdaar queue))
     (setf (car queue)
           (cons (cons obj priority) (car queue))))

    ;; append an element to the back of the queue
    ((>= priority (cdadr queue))
     (setf (cdr (cdr queue)) (list (cons obj priority))
           (cdr queue) (cdr (cdr queue))))

    ;; add an element to its appropriate place
    (t
     (loop for elements on (car queue)
           if (< priority (cdadr elements))
             do (setf (cdr elements) (cons (cons obj priority) (cdr elements)))
                (return t))))

  (car queue))


(defun dequeue (queue)
  "Pops an item off the queue"
  (car (pop (car queue))))


(defun empty? (queue)
  "Peeks at the first element on the queue"
  (null (car queue)))


(defmacro pos (x y)
  "Creates a position"
  `(cons ,x ,y))


(defmacro x (position)
  "Returns the x-coordinate of a position"
  `(car ,position))


(defmacro y (position)
  "Returns the y-coordinate of a position"
  `(cdr ,position))


(defmacro lookup (position graph)
  "Performs a lookup on the grid of the position"
  `(aref ,graph (cdr ,position) (car ,position)))


(defun neighbors (position graph)
  "Gives a list of cardinal neighbors of a position"
  (loop for offset from -1 to 1
        for x = (clamp (+ (x position) offset) 0 (1- (array-dimension graph 1)))
        for y = (clamp (+ (y position) offset) 0 (1- (array-dimension graph 0)))
        if (not (= x (x position)))
          collect (pos x (y position))
        if (not (= y (y position)))
          collect (pos (x position) y)))


(defun cavern-start (graph)
  "Finds the start position of the graph"
  (declare (ignore graph))
  (pos 0 0))


(defun cavern-end (graph)
  "Finds the end position of the graph"
  (apply #'cons (mapcar #'1- (array-dimensions graph))))


(defun dijkstras-path (cavern start end)
  "Attempts to find the path via breadth-first search"
  (loop with frontier = (make-queue start 0)
        with cost = (acons start 0 nil)
        with breadcrumbs = (acons start nil nil)
        while (not (empty? frontier))
        for current = (dequeue frontier)
        for current-cost = (cdr (assoc current cost :test #'equal))
        while (not (equal current end))

        do (loop for neigh in (neighbors current cavern)
                 for neigh-cost = (+ current-cost
                                     (lookup neigh cavern))
                 for recorded-cost = (cdr (assoc neigh cost :test #'equal))
                 if (or (null recorded-cost)
                        (< neigh-cost recorded-cost))
                   do (enqueue neigh neigh-cost frontier)
                      (setf cost (acons neigh neigh-cost cost)
                            breadcrumbs (acons neigh current breadcrumbs)))

        finally (return (loop with path = nil
                              for current = end then (cdr (assoc current
                                                                 breadcrumbs
                                                                 :test #'equal))
                              while (not (null current))
                              do (push current path)
                              finally (return path)))))


(defun sum-path (cavern path)
  "Sums the total cost of a path through a weighted graph (not counting the starting position)"
  (loop for node in (cdr path)
        sum (lookup node cavern)))


(defun find-shortest-cost-path (input)
  "Takes a weighted graph designation and returns the cost of the shortest cost path through"
  (let ((cavern (carve-cave input)))
    (sum-path cavern
              (dijkstras-path cavern
                              (cavern-start cavern)
                              (cavern-end cavern)))))


;;; Part Two
(defun build-full-map (input &key (tile-width 5) (tile-height 5))
  "Takes a string input, gets the initial map, then produces the full map"
  (loop with cavern = (carve-cave input)
        with cavern-prime = (make-array (mapcar (curry #'* 5) (array-dimensions cavern)))
        for row-scalar from 0 below tile-height
        do (loop for col-scalar from 0 below tile-width
                 do (loop for row from 0 below (array-dimension cavern 0)
                          do (loop for col from 0 below (array-dimension cavern 1)
                                   do (setf (aref cavern-prime
                                                  (+ (* row-scalar (array-dimension cavern 0))
                                                     row)
                                                  (+ (* col-scalar (array-dimension cavern 1))
                                                     col))
                                            (1+ (mod (1- (+ row-scalar
                                                            col-scalar
                                                            (aref cavern row col)))
                                                     9))))))
        finally (return cavern-prime)))


(defun a*-path (cavern start end)
  "Attempts to find the path via breadth-first search"
  (loop with frontier = (make-queue start 0)
        with cost = (acons start 0 nil)
        with breadcrumbs = (acons start nil nil)
        while (not (empty? frontier))
        for current = (dequeue frontier)
        for current-cost = (cdr (assoc current cost :test #'equal))
        while (not (equal current end))

        do (loop for neigh in (neighbors current cavern)
                 for neigh-cost = (+ current-cost
                                     (lookup neigh cavern))
                 for recorded-cost = (cdr (assoc neigh cost :test #'equal))
                 if (or (null recorded-cost)
                        (< neigh-cost recorded-cost))
                   do (enqueue neigh (+ neigh-cost
                                        (abs (- (x end) (x neigh)))
                                        (abs (- (y end) (y neigh))))
                               frontier)
                      (setf cost (acons neigh neigh-cost cost)
                            breadcrumbs (acons neigh current breadcrumbs)))

        finally (return (loop with path = nil
                              for current = end then (cdr (assoc current
                                                                 breadcrumbs
                                                                 :test #'equal))
                              while (not (null current))
                              do (push current path)
                              finally (return path)))))


(defun find-shortest-cost-path-full (input)
  "Builds a full map before finding the shortest path and calculating its cost sum"
  ;; XXX: Build patterns from repeating tilesets, use a less cons-y heap
  (let ((cavern (build-full-map input)))
    (sum-path cavern
              (a*-path cavern
                       (cavern-start cavern)
                       (cavern-end cavern)))))
