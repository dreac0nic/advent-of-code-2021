;;;; day-16.lisp

(in-package #:day-16)

(defvar *example-packet* "D2FE28"
  "A standalone packet demonstrating the structure of literals (type ID 4)")

(defvar *example-operator-v1* "38006F45291200"
  "Simple operator packaged used as a structural example of type length ID 0")

(defvar *example-operator-v7* "EE00D40C823060"
  "Very simple operator packet used as a structural example of type length ID 1")

(defvar *example-operator-v4* "8A004A801A8002F478"
  "Represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.")

(defvar *example-operator-v3* "620080001611562C8802118E34"
  "Represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.")

(defvar *example-operator-v3-alt* "C0015000016115A2E0802F182340"
  "Has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.")

(defvar *example-operator-recur* "A0016C880162017C3686B18A3D4780"
  "Is an operator packet that contains an operator packet that contains an operator packet that contains five literal values; it has a version sum of 31.")

(defvar *puzzle-input* nil)

;;; Parsing Input
(defmacro take-bits (count packet)
  "Pops COUNT number of bits off the PACKET and returns the value as an integer"
  `(parse-integer (coerce (mapcar #'digit-char
                                  (loop repeat ,count
                                        collect (pop ,packet)))
                          'string)
                  :radix 2))


(defun egress-packet (packet-string)
  "Takes a packet and turns it into something that can be iterated over"
  (multiple-value-bind (packet-data width)
      (parse-integer packet-string :radix 16)
    (mapcar #'digit-char-p
            (coerce (format nil (format nil "~~~D,'0B" (* width 4)) packet-data)
                    'list))))


(defun parse-packet (packet)
  "Takes a packet and parses it lol"
  ;; XXX: should really use keywords, not symbols; ah well
  (let* ((version (take-bits 3 packet))
         (type-id (take-bits 3 packet))
         (type (cond
                 ((= type-id 0) :sum)
                 ((= type-id 1) :product)
                 ((= type-id 2) :minimum)
                 ((= type-id 3) :maximum)
                 ((= type-id 4) :literal)
                 ((= type-id 5) :greater-than)
                 ((= type-id 6) :less-than)
                 ((= type-id 7) :equals)))
         (parsed-packet (list (cons 'version version)
                              (cons 'type-id type-id)
                              (cons 'type type)))
         (consumed-bits 6))
    (if (eql type :literal)
        ;; literal value
        (setf parsed-packet
              (acons 'value
                     (loop with value = 0
                           for chunkp = (take-bits 1 packet)
                           for chunk = (take-bits 4 packet)
                           do (setf value (+ (ash value 4) chunk)
                                    consumed-bits (+ consumed-bits 5))
                           while (not (zerop chunkp))
                           finally (return value))
                     parsed-packet))

        ;; all other operators
        (let ((length-type (take-bits 1 packet)))
           (setf parsed-packet
                 (acons 'length-type-id length-type parsed-packet))
           (incf consumed-bits) ; note consumption of length type id

           (setf parsed-packet
                 (acons 'sub-packets
                        (if (zerop length-type)
                            (let ((total-sub-packet-length (take-bits 15 packet)))
                              (setf parsed-packet
                                    (acons 'sub-packets-bit-length total-sub-packet-length
                                           parsed-packet))
                              (incf consumed-bits (+ 15 total-sub-packet-length))

                              (loop with bits-remaining = total-sub-packet-length
                                    collect (multiple-value-bind (sub-packet bits)
                                                (parse-packet packet)
                                              (decf bits-remaining bits)
                                              (take-bits bits packet)
                                              sub-packet)
                                    while (> bits-remaining 0)))
                            (let ((sub-packets-count (take-bits 11 packet)))
                              (setf parsed-packet
                                    (acons 'sub-packets-count sub-packets-count
                                           parsed-packet))
                              (incf consumed-bits 11)

                              (loop repeat sub-packets-count
                                    collect (multiple-value-bind (sub-packet bits)
                                                (parse-packet packet)
                                              (take-bits bits packet)
                                              (incf consumed-bits bits)
                                              sub-packet))))
                        parsed-packet))))
    (values parsed-packet consumed-bits)))

;;; Part One
(defun sum-versions (packet)
  "Recursively visits each packet in the tree and returns the sum of their versions"
  (if (not (assoc 'sub-packets packet))
      (cdr (assoc 'version packet))
      (+ (cdr (assoc 'version packet))
         (loop for sub-packet in (cdr (assoc 'sub-packets packet))
               sum (sum-versions sub-packet)))))


(defun packet-version-sum (packet-string)
  "Takes a hex-encoded packet, parses it, and sums their version values"
  (sum-versions (parse-packet (egress-packet packet-string))))

;;; Part Two
(defun eval-packet (packet)
  "Evaluates the packet based on the designated operator"
  (let ((type (cdr (assoc 'type packet))))
    (if (eql type :literal)
        (cdr (assoc 'value packet))
        (apply (case type
                 (:sum #'+)
                 (:product #'*)
                 (:minimum #'min)
                 (:maximum #'max)
                 (:greater-than (lambda (left right)
                                  (if (> left right) 1 0)))
                 (:less-than (lambda (left right)
                               (if (< left right) 1 0)))
                 (:equals (lambda (left right)
                            (if (= left right) 1 0))))
               (mapcar #'eval-packet
                       (cdr (assoc 'sub-packets packet)))))))


(defun eval-bits (packet-string)
  "Evaluates a BITS transmission and returns the result"
  (-> packet-string
      egress-packet
      parse-packet
      eval-packet))
