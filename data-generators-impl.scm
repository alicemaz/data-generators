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

(define-syntax generator
  (syntax-rules ()
    ((_ ?body ...)
     (lambda () ?body ...))))

;; take amount elements from gen and return it in a list
(define (<-* amount gen)
  (map (lambda _ (gen)) (iota amount)))

(define (<- gen)
  (gen))

(define gen-current-fixnum-min (make-parameter -65536))
(define gen-current-fixnum-max (make-parameter 65536))

(define (%random-fixnum lo hi)
  (let ((range (- hi lo -1)))
    (inexact->exact (+ (bsd:random-integer range) lo))))

;; since these generators
;; will most likely be used to feed some foreign code
;; the ranges have been selected to conform to those present on most platforms

(define gen-fixnum
  (case-lambda
    (()      (gen-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((upper) (gen-fixnum (gen-current-fixnum-min) upper))
    ((lower upper)
     (unless (<= lower upper)
       (error "upper bound must be <= lower bound" lower upper))
     (generator (%random-fixnum lower upper)))))

(define-syntax define-fixed-range-generator
  (syntax-rules ()
    ((_ ?name ?lower ?upper)
     (define (?name) (generator (%random-fixnum ?lower ?upper))))))

(define-fixed-range-generator gen-int8 -127 127)
(define-fixed-range-generator gen-uint8 0 255)
(define-fixed-range-generator gen-int16 -32767 32767)
(define-fixed-range-generator gen-uint16 0 65535)
(define-fixed-range-generator gen-int32 -2147483647 2147483647)
(define-fixed-range-generator gen-uint32 0 4294967295)
(define-fixed-range-generator gen-int64 -9223372036854775807 9223372036854775807)
(define-fixed-range-generator gen-uint64 0 18446744073709551615)

(define (%clamp val lower upper)
  (cond
   ((> val upper) upper)
   ((and (> val lower) (<= val upper)) val)
   (else lower)))

(define (%random-real #!optional (size 1.0) (start 0.0))
  (let ((ub (+ size start)))
    (%clamp (+ start (* size (bsd:random-real))) start ub)))

(define gen-real
  (case-lambda
    (()      (gen-real 0.0 1.0))
    ((upper) (gen-real 0.0 upper))
    ((lower upper)
     (unless (<= lower upper)
       (error "lower bound must be <= upper bound" lower upper))
     (generator (%random-real (- upper lower) lower)))))

(define (gen-bool)
  (generator (zero? (bsd:random-fixnum 2))))

(define (%random-char charset)
  (let loop ((cursor (char-set-cursor charset)) (i (bsd:random-fixnum (sub1 (char-set-size charset)))))
    (cond
     ((zero? i) (char-set-ref charset cursor))
     (else (loop (char-set-cursor-next charset cursor) (sub1 i))))))

(define (boundaries->charset lower upper)
  (define (maybe-apply v p t) (if (p v) (t v) v))

  (let ((lo (max 0 (maybe-apply lower char? char->integer)))
        (hi (max 0 (min 255 (maybe-apply upper char? char->integer)))))
    (unless (<= lo hi)
      (error 'sizer->charset "lower bound must be <= upper bound" lower upper))
    (list->char-set (map integer->char (iota (add1 (- hi lo)) lo)))))

(define gen-char
  (case-lambda
    (() (gen-char char-set:graphic))
    ((charset)
     (unless (char-set? charset)
       (error "You need to supply a char-set"))
     (generator (%random-char charset)))
    ((lower upper)
     (let ((charset (boundaries->charset lower upper)))
       (generator (%random-char charset))))))

;; ;; combinators
;; (define gen-current-default-size (make-parameter (cut gen-uint8)))

;; (define-syntax with-size
;;   (syntax-rules ()
;;     ((_ (lb . ub) body0 ...)
;;      (parameterize ((gen-current-default-size (cut between gen-fixnum lb ub)))
;;        body0 ...))
;;     ((_ size body0 ...)
;;      (parameterize ((gen-current-default-size (constantly size)))
;;        body0 ...))))

;; (define (gen-sample-of list-of-gen)
;;   (let* ((l   (length list-of-gen))
;;          (gen (list-ref list-of-gen  (between gen-fixnum 0 (sub1 l)))))
;;     (gen)))

;; (define (gen-pair-of gen1 gen2)
;;   (cons (gen1) (gen2)))

;; (define (gen-tuple-of . gens)
;;   (map (lambda (g) (g)) gens))

;; (define gen-list-of
;;   (case-lambda
;;     ((gen) (gen-list-of gen (gen-current-default-size)))
;;     ((gen size)
;;      (list-tabulate (size) (lambda _ (gen))))))

;; (define  gen-alist-of
;;   (case-lambda
;;     ((key-gen value-gen) (gen-list-of (lambda () (gen-pair-of key-gen value-gen)) (gen-current-default-size)))
;;     ((key-gen value-gen size)
;;      (gen-list-of (lambda () (gen-pair-of key-gen value-gen)) size))))

;; (define gen-vector-of
;;   (case-lambda
;;     ((gen) (gen-vector-of gen (gen-current-default-size)))
;;     ((gen size)
;;      (let ((size (size)))
;;        (do ((i 0 (add1 i))
;;             (vec (make-vector size)))
;;            ((>= i size) vec)
;;          (vector-set! vec i (gen)))))))

;; (define gen-string-of
;;   (case-lambda
;;     (()    (gen-string-of char-set:graphic (gen-current-default-size)))
;;     ((cs) (gen-string-of cs (gen-current-default-size)))
;;     ((cs size)
;;      (let ((size (size)))
;;        (with-output-to-string
;;          (lambda ()
;;            (do ((i 0 (add1 i)))
;;                ((>= i size))
;;              (display (gen-char cs)))))))))

;; (define gen-hash-table-of
;;   (case-lambda
;;     ((key-gen value-gen) (gen-hash-table-of key-gen value-gen (gen-current-default-size)))
;;     ((key-gen value-gen size)
;;      (let ((size (size)))
;;        (do ((i 0 (add1 i))
;;             (ht (make-hash-table)))
;;            ((>= i size) ht)
;;          (hash-table-set! ht (key-gen) (value-gen)))))))

;; (define-syntax gen->sequence
;;   (syntax-rules ()
;;     ((_ amount body0 ...)
;;      (lambda (proc)
;;        (do ((i 1 (add1 i)))
;;            ((>= i amount))
;;          (proc (begin body0 ...)))))))

;; (define (run-sequence seq proc) (seq proc))
