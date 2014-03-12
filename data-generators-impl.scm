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


;; ranges are used to configure some generators
(define (range start stop)
  (cond
   ((and (not start) stop) (cons (gen-current-fixnum-min) stop))
   ((and start (not stop)) (cons start (gen-current-fixnum-max)))
   (else (cons start stop))))

(define range? pair?)
(define range-start car)
(define range-end cdr)

;; generator implementation
(define-syntax generator
  (syntax-rules ()
    ((_ ?body ...)
     (lambda () ?body ...))))

(define generator? procedure?)

(define (maybe-apply v p t) (if (p v) (t v) v))

(define type-generator-map '())

(define (register-generator-for-type! type-pred gen)
  (set! type-generator-map (cons (cons type-pred gen) type-generator-map)))

(define (find-generator-for value)
  (let ((match (find (lambda (p) ((car p) value)) type-generator-map)))
    (if match (cdr match) #f)))

(define (range->generator rng)
  (let ((gen (find-generator-for (range-start rng))))
    (unless gen
      (error "Could not find generator for given range. Did you register one for that type?" rng))
    (gen rng)))

;; convenience procedure to quickly create a generator for a given range that dispatches on
;; the type of the arguments
(define gen
  (case-lambda
    ((lower upper)
     (range->generator (range lower upper)))
    ((range)
     (range->generator range))))

;; accessing elements from a generator
(define <-
  (case-lambda
    ((gen) (gen))
    ((amount gen)
     (list-tabulate amount (lambda _ (gen))))))

(define (gen-for-each rounds proc gen)
  (do ((i 1 (add1 i)))
      ((>= i rounds))
    (proc (<- gen))))

;; primitives
(define (gen-constant value)
  (generator value))

(define gen-current-fixnum-min (make-parameter -65536))
(define gen-current-fixnum-max (make-parameter 65536))

(define (%random-fixnum lo hi)
  (let ((range (- hi lo -1)))
    (inexact->exact (+ (bsd:random-integer range) lo))))

(define (size-spec->bounds size-spec #!optional (lower (gen-current-fixnum-min)))
  (cond
   ((range? size-spec)
    (list (range-start size-spec) (range-end size-spec)))
   ((number? size-spec)
    (list lower size-spec))
   (else (error "invalid size specification" size-spec))))

(define gen-fixnum
  (case-lambda
    (()
     (gen-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((size-spec)
     (apply gen-fixnum (size-spec->bounds size-spec)))
    ((lower upper)
     (unless (<= lower upper)
       (error "lower bound must be <= upper bound" lower upper))
     (generator (%random-fixnum lower upper)))))

(register-generator-for-type! fixnum? gen-fixnum)

(define gen-odd-fixnum
  (case-lambda
    (()
     (gen-odd-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((size-spec)
     (apply gen-odd-fixnum (size-spec->bounds size-spec)))
    ((lower upper)
     (unless (<= lower upper)
       (error "lower bound must be <= upper bound" lower upper))
     (let ((lower (if (odd? lower) lower (+ 1 lower)))
           (upper (if (odd? upper) upper (- upper 1))))
       (generator
        (let ((val (%random-fixnum lower upper)))
          (if (odd? val) val (+ 1 val))))))))

(define gen-even-fixnum
  (case-lambda
    (()
     (gen-even-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((size-spec)
     (apply gen-even-fixnum (size-spec->bounds size-spec)))
    ((lower upper)
     (unless (<= lower upper)
       (error "lower bound must be <= upper bound" lower upper))
     (let ((lower (if (even? lower) lower (+ 1 lower)))
           (upper (if (even? upper) upper (- upper 1))))
       (generator
        (let ((val (%random-fixnum lower upper)))
          (if (even? val) val (+ 1 val))))))))

(define-syntax define-fixed-range-generator
  (syntax-rules ()
    ((_ ?name ?lower ?upper)
     (define (?name) (generator (%random-fixnum ?lower ?upper))))))


;; since these generators
;; will most likely be used to feed some foreign code
;; the ranges have been selected to conform to those present on most platforms

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
    (()
     (gen-real 0.0 1.0))
    ((size-spec)
     (apply gen-real (size-spec->bounds size-spec 0.0)))
    ((lower upper)
     (unless (<= lower upper)
       (error "lower bound must be <= upper bound" lower upper))
     (generator (%random-real (- upper lower) lower)))))

(register-generator-for-type! flonum? gen-real)

(define (gen-bool)
  (generator (zero? (bsd:random-fixnum 2))))

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

(register-generator-for-type! char? gen-char)

(define (gen-sample candidates)
  (let ((l (length candidates)))
    (generator  (list-ref candidates (<- (gen-fixnum 0 (sub1 l)))))))


;; combinators
(define gen-current-default-size (make-parameter (gen-uint8)))

(define-syntax with-size
  (syntax-rules ()
    ((_ size-spec body0 ...)
     (parameterize ((gen-current-default-size (size-spec->gen size-spec)))
       body0 ...))))

(define (gen-values-of . gens)
  (generator
   (apply values (map <- gens))))

(define (gen-sample-of . gens)
  (let ((l (length gens))
	(gens (list->vector gens)))
    (generator (<- (vector-ref gens (<- (gen-fixnum 0 (sub1 l))))))))

(define (gen-pair-of car-gen cdr-gen)
  (generator
   (cons (<- car-gen) (<- cdr-gen))))

(define (gen-tuple-of . gens)
  (generator (map <- gens)))

(define (size-spec->gen spec)
  (cond
   ((generator? spec) spec)
   ((range? spec)    (gen-fixnum (range-start spec) (range-end spec)))
   ((fixnum? spec)   (gen-constant spec))
   (else (error "Invalid size specification" spec))))

(define gen-list-of
  (case-lambda
    ((gen) (gen-list-of gen (gen-current-default-size)))
    ((gen size-spec)
     (let ((size-gen (size-spec->gen size-spec)))
       (generator
	(list-tabulate (<- size-gen) (lambda _ (<- gen))))))))

(define  gen-alist-of
  (case-lambda
    ((key-gen value-gen)
     (gen-list-of (gen-pair-of key-gen value-gen) (gen-current-default-size)))
    ((key-gen value-gen size-spec)
     (gen-list-of (gen-pair-of key-gen value-gen) size-spec))))

(define gen-string-of
  (case-lambda
    ((gen)  (gen-string-of gen (gen-current-default-size)))
    ((gen size-spec)
     (generator
      (list->string (<- (gen-list-of gen size-spec)))))))

(define gen-vector-of
  (case-lambda
    ((gen) (gen-vector-of gen (<- (gen-current-default-size))))
    ((gen size-spec)
     (let ((size-gen (size-spec->gen size-spec)))
       (generator
	(let ((size (<- size-gen)))
	  (do ((i 0 (add1 i))
	       (vec (make-vector size)))
	      ((>= i size) vec)
	    (vector-set! vec i (gen)))))))))

(define gen-hash-table-of
  (case-lambda
    ((key-gen value-gen) (gen-hash-table-of key-gen value-gen (<- (gen-current-default-size))))
    ((key-gen value-gen size-spec)
     (let ((size-gen (size-spec->gen size-spec)))
       (generator
	(let ((size (<- size-gen)))
	  (do ((i 0 (add1 i))
	       (ht (make-hash-table)))
	      ((>= i size) ht)
	    (hash-table-set! ht (<- key-gen) (<- value-gen)))))))))

(define (gen-record ctor . slot-gens)
  (generator (apply ctor (map <- slot-gens))))

(define (gen-transform transformer gen)
  (generator
   (transformer (<- gen))))
