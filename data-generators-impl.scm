;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.


(define gen-current-fixnum-min (make-parameter -65536))
(define gen-current-fixnum-max (make-parameter 65536))

(define (%random-fixnum lo hi)
  (unless (<= lo hi)
    (error '%random-fixnum "upper bound must be <= lower bound" lo hi))
  (let ((range (- hi lo -1)))
    (inexact->exact (+ (bsd:random-integer range) lo))))

;; since these generators
;; will most likely be used to feed some foreign code
;; the ranges have been selected to conform to those present on most platforms

(define gen-int8   (cut %random-fixnum -127 127))
(define gen-uint8  (cut %random-fixnum 0 255))
(define gen-int16  (cut %random-fixnum -32767 32767))
(define gen-uint16 (cut %random-fixnum 0 65535))
(define gen-int32  (cut %random-fixnum -2147483647 2147483647))
(define gen-uint32 (cut %random-fixnum 0 4294967295))
(define gen-int64  (cut %random-fixnum -9223372036854775807 9223372036854775807))
(define gen-uint64 (cut %random-fixnum 0 18446744073709551615))


;; generic generator for fixnum allows to size the resulting number
(define make-sizer cons)
(define sizer/lb car)
(define sizer/ub cdr)

(define (between  gen lb ub) (gen (make-sizer lb ub)))
(define (at-most  gen ub)    (gen (make-sizer (gen-current-fixnum-min) ub)))
(define (at-least gen lb)    (gen (make-sizer lb (gen-current-fixnum-max))))

(define (gen-fixnum #!optional (sizer (make-sizer (gen-current-fixnum-min) (gen-current-fixnum-max))))
  (%random-fixnum (sizer/lb sizer) (sizer/ub sizer)))

(define (%clamp val lower upper)
  (cond
   ((> val upper) upper)
   ((and (> val lower) (<= val upper)) val)
   (else lower)))

(define (%random-real #!optional (size 1.0) (start 0.0))
  (let ((ub (+ size start)))
    (%clamp (+ start (* size (bsd:random-real))) start ub)))

(define (gen-real #!optional (sizer (make-sizer 0.0 1.0)))
  (let* ((start (sizer/lb sizer))
         (size  (- (sizer/ub sizer) start)))
    (%random-real size start)))

(define (gen-bool) (zero? (bsd:random-fixnum 2)))

(define (%random-char charset)
  (let loop ((cursor (char-set-cursor charset)) (i (bsd:random-fixnum (sub1 (char-set-size charset)))))
    (cond
     ((zero? i) (char-set-ref charset cursor))
     (else (loop (char-set-cursor-next charset cursor) (sub1 i))))))



(define (sizer->charset sizer)
  (define (maybe-apply v p t) (if (p v) (t v) v))

  (let ((lo (max 0 (maybe-apply (sizer/lb sizer) char? char->integer)))
        (hi (max 0 (min 255 (maybe-apply (sizer/ub sizer) char? char->integer)))))
    (unless (<= lo hi)
      (error 'sizer->charset "lower bound must be <= upper bound"))
    (list->char-set (map integer->char (iota (add1 (- hi lo)) lo)))))

(define (gen-char #!optional (sizer-or-charset char-set:graphic))
  (if (char-set? sizer-or-charset)
      (%random-char sizer-or-charset)
      (%random-char (sizer->charset sizer-or-charset))))

;; combinators
(define gen-current-default-size (make-parameter (cut gen-uint8)))

(define-syntax with-size
  (syntax-rules ()
    ((_ (lb . ub) body0 ...)
     (parameterize ((gen-current-default-size (cut between gen-fixnum lb ub)))
       body0 ...))
    ((_ size body0 ...)
     (parameterize ((gen-current-default-size (constantly size)))
       body0 ...))))

(define (gen-sample-of list-of-gen)
  (let* ((l   (length list-of-gen))
         (gen (list-ref list-of-gen  (between gen-fixnum 0 (sub1 l)))))
    (gen)))

(define (gen-pair-of gen1 gen2)
  (cons (gen1) (gen2)))

(define (gen-tuple-of . gens)
  (map (lambda (g) (g)) gens))

(define gen-list-of
  (case-lambda
    ((gen) (gen-list-of gen (gen-current-default-size)))
    ((gen size)
     (list-tabulate (size) (lambda _ (gen))))))

(define  gen-alist-of
  (case-lambda
    ((key-gen value-gen) (gen-list-of (lambda () (gen-pair-of key-gen value-gen)) (gen-current-default-size)))
    ((key-gen value-gen size)
     (gen-list-of (lambda () (gen-pair-of key-gen value-gen)) size))))

(define gen-vector-of
  (case-lambda
    ((gen) (gen-vector-of gen (gen-current-default-size)))
    ((gen size)
     (let ((size (size)))
       (do ((i 0 (add1 i))
            (vec (make-vector size)))
           ((>= i size) vec)
         (vector-set! vec i (gen)) vec)))))

(define gen-string-of
  (case-lambda
    (()    (gen-string-of char-set:graphic (gen-current-default-size)))
    ((cs) (gen-string-of cs (gen-current-default-size)))
    ((cs size)
     (let ((size (size)))
       (with-output-to-string
         (lambda ()
           (do ((i 0 (add1 i)))
               ((>= i size))
             (display (gen-char cs)))))))))

(define gen-hash-table-of
  (case-lambda
    ((key-gen value-gen) (gen-hash-table-of key-gen value-gen (gen-current-default-size)))
    ((key-gen value-gen size)
     (let ((size (size)))
       (do ((i 0 (add1 i))
            (ht (make-hash-table)))
           ((>= i size) ht)
         (hash-table-set! ht (key-gen) (value-gen)))))))
