(module current-impl
  (run-benchmark)

  (import chicken scheme)
  (use benchmark srfi-14 srfi-69 srfi-1 (prefix random-bsd bsd:) numbers)

  (include "../data-generators-impl.scm")

  (define (%random-char charset)
    (let loop ((cursor (char-set-cursor charset)) (i (bsd:random-fixnum (sub1 (char-set-size charset)))))
      (cond
       ((zero? i) (char-set-ref charset cursor))
       (else (loop (char-set-cursor-next charset cursor) (sub1 i))))))

  (define (boundaries->charset lower upper)
    (let ((lo (max 0 (maybe-apply lower char? char->integer)))
          (hi (max 0 (min 255 (maybe-apply upper char? char->integer)))))
      (unless (<= lo hi)
        (error 'sizer->charset "lower bound must be <= upper bound" lower upper))
      (list->char-set (map integer->char (iota (add1 (- hi lo)) lo)))))

  (define gen-char
    (case-lambda
      (() (gen-char char-set:graphic))
      ((charset-or-range)
       (cond
        ((char-set? charset-or-range)
         (generator (%random-char charset-or-range)))
        ((range? charset-or-range)
         (gen-char (range-start charset-or-range) (range-end charset-or-range)))
        (else (error "Invalid argument. Must be either range or charset" charset-or-range))))
      ((lower upper)
       (unless (char<=? lower upper)
         (error "lower bound must be <= upper bound" lower upper))
       (let ((charset (boundaries->charset lower upper)))
         (generator (%random-char charset))))))

  (define gen-string-of
    (case-lambda
      ((gen)  (gen-string-of gen (gen-current-default-size)))
      ((gen size-spec)
       (generator
        (list->string (<- (gen-list-of gen size-spec)))))))


  (define (run-benchmark)
    (let ((charset-letter-digit (gen-string-of (gen-char char-set:letter+digit) 100))
          (charset-all          (gen-string-of (gen-char char-set:full) 100))
          (charset-custom       (gen-string-of (gen-char #\a #\z) 100)))
      (list
       (benchmark-run (lambda () (<- charset-letter-digit)))
       (benchmark-run (lambda () (<- charset-all)))
       (benchmark-run (lambda () (<- charset-custom))))))

  )

(module test-impl
  (run-benchmark)
  (import chicken scheme)
  (use benchmark srfi-14 srfi-69 srfi-1 (prefix random-bsd bsd:) data-structures ports numbers)

  (include "../data-generators-impl.scm")

  (define char-set->vector (o list->vector char-set->list))

  (define (boundaries->char-vector lower upper)
    (let ((lo (max 0 (maybe-apply lower char? char->integer)))
          (hi (max 0 (min 255 (maybe-apply upper char? char->integer)))))
      (unless (<= lo hi)
        (error 'sizer->charset "lower bound must be <= upper bound" lower upper))
      (let ((size (add1 (- hi lo))))
        (do ((char lo (add1 char))
             (i 0 (add1 i))
             (vec (make-vector size)))
            ((> char hi) vec)
          (vector-set! vec i (integer->char char))))))

  (define gen-char
    (let ((%char-gen (lambda (char-vec)
                       (let ((index-gen (gen-fixnum 0 (sub1 (vector-length char-vec)))))
                         (generator
                          (vector-ref char-vec (<- index-gen)))))))
      (case-lambda
        (() (gen-char char-set:graphic))
        ((charset-or-range)
         (cond
          ((char-set? charset-or-range)
           (%char-gen (char-set->vector charset-or-range)))
          ((range? charset-or-range)
           (gen-char (range-start charset-or-range) (range-end charset-or-range)))
          (else (error "Invalid argument. Must be either range or charset" charset-or-range))))
        ((lower upper)
         (unless (char<=? lower upper)
           (error "lower bound must be <= upper bound" lower upper))
         (%char-gen (boundaries->char-vector lower upper))))))

  (define gen-string-of
    (case-lambda
      ((gen)  (gen-string-of gen (gen-current-default-size)))
      ((gen size-spec)
       (let ((size-gen (size-spec->gen size-spec)))
         (generator
          (list->string (<- (<- size-gen) gen)))))))

  (define (run-benchmark)
    (let ((charset-letter-digit (gen-string-of (gen-char char-set:letter+digit) 100))
          (charset-all          (gen-string-of (gen-char char-set:full) 100))
          (charset-custom       (gen-string-of (gen-char #\a #\z) 100)))
      (list
       (benchmark-run (lambda () (<- charset-letter-digit)))
       (benchmark-run (lambda () (<- charset-all)))
       (benchmark-run (lambda () (<- charset-custom))))))
)


(define (report title result)
  (print title)
  (print "==============================")
  (print "letter+digit: " (car result))
  (print "all: " (cadr result))
  (print "custom: " (caddr result)))

(import (prefix current-impl current:))
(report "Current" (current:run-benchmark))

(print "\n\n")

(import (prefix test-impl test:))
(report "Test" (test:run-benchmark))
