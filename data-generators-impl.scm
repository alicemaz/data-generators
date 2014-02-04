(use (prefix random-bsd bsd:) (only srfi-1 iota list-tabulate) srfi-14 srfi-69)

(define current-fixnum-min (make-parameter most-negative-fixnum))
(define current-fixnum-max (make-parameter most-positive-fixnum))

(define (%random-fixnum lo hi)
  (unless (<= lo hi)
    (error '%random-fixnum "upper bound must be <= lower bound" lo hi))
    (+ lo (bsd:random-integer (+ 1 (- hi lo)))))

(define (%gen-fixnum/fixed-range size #!optional (start 0))
  (lambda ()
    (+ (bsd:random-integer size) start)))

;; these procedures can not be sized
(define gen-int8   (%gen-fixnum/fixed-range 256 -128))
(define gen-uint8  (%gen-fixnum/fixed-range 256))
(define gen-int16  (%gen-fixnum/fixed-range 65536 -32768))
(define gen-uint16 (%gen-fixnum/fixed-range 65536))
(define gen-int32  (%gen-fixnum/fixed-range (expt 2 32) (- (expt 2 31))))
(define gen-uint32 (%gen-fixnum/fixed-range (expt 2 32)))
(define gen-int64  (%gen-fixnum/fixed-range (expt 2 64) (- (expt 2 64))))
(define gen-uint64 (%gen-fixnum/fixed-range (expt 2 64)))

;; generic generator for fixnum allows to size the resulting number
(define make-sizer cons)
(define sizer/lb car)
(define sizer/ub cdr)

(define (between gen lb ub)  (gen (make-sizer lb ub)))
(define (at-most gen  ub)    (gen (make-sizer (current-fixnum-min) ub)))
(define (at-least gen lb)    (gen (make-sizer lb (current-fixnum-max))))
(define (exactly gen lb ub)  (gen (make-sizer lb ub)))

(define (gen-fixnum #!optional (sizer (make-sizer (current-fixnum-min) (current-fixnum-max))))
  (%random-fixnum (sizer/lb sizer) (sizer/ub sizer)))

(define (gen-bool) (zero? (bsd:random-fixnum 2)))

(define (%random-char charset)
  (let loop ((cursor (char-set-cursor charset)) (i (bsd:random-fixnum (sub1 (char-set-size charset)))))
    (cond
     ((zero? i) (char-set-ref charset cursor))
     (else (loop (char-set-cursor-next charset cursor) (sub1 i))))))

(define (sizer->charset sizer)
  (let ((lo (char->integer (sizer/lb sizer)))
        (hi (char->integer (sizer/ub sizer))))
    (unless (<= lo hi)
      (error 'sizer->charset "lower bound must be <= upper bound"))
    (list->char-set (map integer->char (iota (add1 (- hi lo)) lo)))))

(define (gen-char #!optional (sizer-or-charset char-set:graphic))
  (if (char-set? sizer-or-charset)
      (%random-char sizer-or-charset)
      (%random-char (sizer->charset sizer-or-charset))))

;; combinators
(define gen-current-default-size (make-parameter (gen-uint8)))

(define (gen-pair-of gen1 gen2)
  (cons (gen1) (gen2)))

(define (gen-tuple-of . gens)
  (map (lambda (g) (g)) gens))

(define gen-list-of
  (case-lambda
    ((gen) (gen-list-of (gen-current-default-size) gen))
    ((size gen) (list-tabulate size (lambda _ (gen))))))

(define  gen-alist-of
  (case-lambda
    ((key-gen value-gen) (gen-list-of (gen-current-default-size) (lambda () (gen-pair-of key-gen value-gen))))
    ((size key-gen value-gen)
     (gen-list-of size (lambda () (gen-pair-of key-gen value-gen))))))

(define gen-vector-of
  (case-lambda
    ((gen) (gen-vector-of (gen-current-default-size) gen))
    ((size gen)
     (do ((i 0 (add1 i))
          (vec (make-vector size)))
         ((>= i size) vec)
       (vector-set! vec i (gen)) vec))))

(define gen-string-of
  (case-lambda
    (()    (gen-string-of (gen-current-default-size) gen-char))
    ((gen) (gen-string-of (gen-current-default-size) gen))
    ((size gen)
     (with-output-to-string
       (lambda ()
         (do ((i 0 (add1 i)))
             ((>= i size))
           (display (gen))))))))

(define gen-hash-table-of
  (case-lambda
    ((key-gen value-gen) (gen-hash-table-of (gen-current-default-size) key-gen value-gen))
    ((size key-gen value-gen)
     (do ((i 0 (add1 i))
          (ht (make-hash-table)))
         ((>= i size) ht)
       (hash-table-set! ht (key-gen) (value-gen))))))
