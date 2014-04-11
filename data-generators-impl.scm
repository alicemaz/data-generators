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


;;== random primitives
;; get a random integer uniformly
(: %random-fixnum ((or fixnum float) (or fixnum float) -> fixnum))
(define (%random-fixnum lo hi)
  (let ((range (- hi lo -1)))
    (inexact->exact (+ (bsd:random-integer range) lo))))

(: %random-real (float float -> float))
(define (%random-real #!optional (size 1.0) (start 0.0))
  (let ((ub (+ size start)))
    (%clamp (+ start (* size (bsd:random-real))) start ub)))

(: %clamp (float float float -> float))
(define (%clamp val lower upper)
  (cond
   ((> val upper) upper)
   ((and (> val lower) (<= val upper)) val)
   (else lower)))


;;== ranges are used to configure some generators
(: range (forall (start (stop *)) (start stop -> (pair start stop))))
(define (range start stop)
  (cond
   ((and (not start) stop) (cons (gen-current-fixnum-min) stop))
   ((and start (not stop)) (cons start (gen-current-fixnum-max)))
   (else (cons start stop))))

(: range? (* -> boolean))
(define range? pair?)

(: range-start (forall (e (p (pair e e))) (p -> e)))
(define range-start car)

(: range-end (forall (e (p (pair e e))) (p -> e)))
(define range-end cdr)

;;== generator implementation
(define-syntax generator
  (syntax-rules ()
    ((_ ?body ...)
     (lambda () ?body ...))))

(define generator? procedure?)

;;== accessing elements from a generator
(define <-
  (case-lambda
    ((gen) (gen))
    ((amount gen)
     (map (lambda _ (gen)) (iota amount)))))

(define (gen-for-each rounds proc gen)
  (do ((i 1 (add1 i)))
      ((>= i rounds))
    (proc (<- gen) i)))

;;== generic generator
;; convenience procedure to quickly create a generator for a given range that dispatches on
;; the type of the arguments
(define gen
  (case-lambda
    ((lower upper)
     (range->generator (range lower upper)))
    ((range)
     (range->generator range))))

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

;;== primitive generators
(define gen-current-fixnum-min (make-parameter -214748364))
(define gen-current-fixnum-max (make-parameter 214748364))

(define (gen-constant value)
  (generator value))

(define (size-spec->bounds size-spec #!optional (lower (gen-current-fixnum-min)))
  (cond
   ((range? size-spec)
    (list (range-start size-spec) (range-end size-spec)))
   (else (error "invalid size specification" size-spec))))

(define-syntax safe-apply-range
  (syntax-rules ()
    ((_ ?proc ?rng ?args ...)
     (begin
       (unless (range? ?rng)
         (error (quote ?proc) "expected range got " ?rng))
       (?proc (range-start ?rng) (range-end ?rng) ?args ...)))))

(define-syntax assert-valid-bounds
  (syntax-rules ()
    ((_ ?lower ?upper)
     (begin
       (unless (and (number? ?lower) (number? ?upper))
         (error "expected two numbers but got " ?lower " and " ?upper))
       (unless (<= ?lower ?upper)
         (error "lower bound must be less or equal than upper bound"))))))

(define gen-fixnum
  (case-lambda
    (()
     (gen-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((range)
     (safe-apply-range gen-fixnum range))
    ((lower upper)
     (assert-valid-bounds lower upper)
     (generator (%random-fixnum lower upper)))))

(define fixnums gen-fixnum)

(register-generator-for-type! fixnum? gen-fixnum)

(define gen-odd-fixnum
  (case-lambda
    (()
     (gen-odd-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((range)
     (safe-apply-range gen-odd-fixnum range))
    ((lower upper)
     (assert-valid-bounds lower upper)
     (let ((lower (if (odd? lower) lower (+ 1 lower)))
           (upper (if (odd? upper) upper (- upper 1))))
       (generator
        (let ((val (%random-fixnum lower upper)))
          (if (odd? val) val (+ 1 val))))))))

(define odd-fixnums gen-odd-fixnum)

(define gen-even-fixnum
  (case-lambda
    (()
     (gen-even-fixnum (gen-current-fixnum-min) (gen-current-fixnum-max)))
    ((range)
     (safe-apply-range gen-even-fixnum range))
    ((lower upper)
     (assert-valid-bounds lower upper)
     (let ((lower (if (even? lower) lower (+ 1 lower)))
           (upper (if (even? upper) upper (- upper 1))))
       (generator
        (let ((val (%random-fixnum lower upper)))
          (if (even? val) val (+ 1 val))))))))

(define even-fixnums gen-even-fixnum)

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
(define-fixed-range-generator gen-int64  -9223372036854775807 9223372036854775807)
(define-fixed-range-generator gen-uint64 0 18446744073709551615)

(define gen-real
  (case-lambda
    (()
     (gen-real 0.0 1.0))
    ((range)
     (safe-apply-range gen-real range))
    ((lower upper)
     (assert-valid-bounds lower upper)
     (generator (%random-real (- upper lower) lower)))))

;; TODO: find better name
(define (gen-flonum-specialties)
  (gen-sample (list +nan.0 +inf.0 -inf.0)))

(define gen-flonum
  (case-lambda
    (()
     (gen-sample-of (gen-flonum-specialties) (gen-flonum 0.0 1.0)))
    ((range)
     (safe-apply-range gen-flonum range))
    ((lower upper)
     (assert-valid-bounds lower upper)
     (generator (%random-real (- upper lower) lower)))))

(define flonums gen-flonum)

(register-generator-for-type! flonum? gen-real)

(define gen-rational
  (case-lambda
    (()
     (gen-rational (fixnums) (fixnums)))
    ((nom-gen denom-gen)
     (let ((ensure-not-zero (lambda (val)
                              (if (zero? val)
                                  (<- (gen-fixnum 1 (gen-current-fixnum-max)))
                                  val))))
       (generator
        (let ((nom (<- nom-gen))
              (denom (ensure-not-zero (<- denom-gen))))
          (/ nom denom)))))))

(define rationals gen-rational)

(define gen-series
  (case-lambda
    (() (gen-series (gen-current-fixnum-min) (gen-current-fixnum-max) add1))
    ((range step)
     (safe-apply-range gen-series range step))
    ((lower upper step)
     (assert-valid-bounds lower upper)
     (let ((next lower))
       (generator
        (let ((actual next))
          (set! next (if (>= actual upper) lower (step actual)))
          (min actual upper)))))))

(: gen-bool (-> (procedure () boolean)))
(define (gen-bool)
  (generator (zero? (bsd:random-fixnum 2))))

(: booleans (-> (procedure () boolean)))
(define booleans gen-bool)

(define char-set->vector (o list->vector char-set->list))

(define (boundaries->char-vector lower upper)
  (define (maybe-apply v p t) (if (p v) (t v) v))

  (let ((lo (max 0 (maybe-apply lower char? char->integer)))
        (hi (max 0 (min 255 (maybe-apply upper char? char->integer)))))
    (unless (<= lo hi)
      (error "lower bound must be <= upper bound" lower upper))
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
       (unless (and (char? lower) (char? upper))
         (error "expected two characters but got " lower " and " upper))
       (unless (char<=? lower upper)
         (error "lower bound must be <= upper bound" lower upper))
       (%char-gen (boundaries->char-vector lower upper))))))

(define chars gen-char)

(register-generator-for-type! char? gen-char)

(define (gen-sample candidates)
  (let* ((candidates (list->vector candidates))
         (index-gen (gen-fixnum 0 (sub1 (vector-length candidates)))))
    (generator (vector-ref candidates (<- index-gen)))))

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
  (let* ((l (length gens))
	(gens (list->vector gens))
        (index-gen (gen-fixnum 0 (sub1 l))))
    (generator (<- (vector-ref gens (<- index-gen))))))

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
        (<- (<- size-gen) gen))))))

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
     (let ((size-gen (size-spec->gen size-spec)))
       (generator
        (list->string (<- (<- size-gen) gen)))))))

(define gen-string gen-string-of)

(define gen-symbol-of
  (case-lambda
    (()
     (gen-symbol-of (gen-char char-set:letter+digit)))
    ((char-gen)
     (gen-transform string->symbol (gen-string-of char-gen)))))

(define gen-symbol gen-symbol-of)

(define gen-keyword-of
  (case-lambda
    (()
     (gen-keyword-of (gen-char char-set:letter+digit)))
    ((char-gen)
     (gen-transform string->keyword (gen-string-of char-gen)))))

(define gen-keyword gen-keyword-of)

(define-syntax define-procedure-generator
  (ir-macro-transformer
   (lambda (exp inj cmp)
     (let ((name (cadr exp))
           (max-arity (caddr exp)))
       `(define ,name
          (case-lambda
            (() (,name (<- (gen-fixnum 0 ,max-arity)) (gen-bool)))
            ((arity return)
             (unless (and (fixnum? arity) (<= arity ,max-arity))
               (error "Arity must be a fixnum between 0 and " ,max-arity))
             (case arity
               ,@(map (lambda (i)
                        `((,i) (generator (lambda ,(list-tabulate i (constantly '_)) (<- return)))))
                      (iota (+  max-arity 1)))))))))))

(define-procedure-generator gen-procedure 20)

(define gen-vector-of
  (case-lambda
    ((gen) (gen-vector-of gen (gen-current-default-size)))
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
    ((key-gen value-gen)
     (gen-hash-table-of key-gen value-gen (gen-current-default-size) eq?))
    ((key-gen value-gen size-spec)
     (gen-hash-table-of key-gen value-gen size-spec eq?))
    ((key-gen value-gen size-spec equal?)
     (let ((size-gen (size-spec->gen size-spec)))
       (generator
	(let ((size (<- size-gen)))
	  (do ((i 0 (add1 i))
	       (ht (make-hash-table equal?)))
	      ((>= i size) ht)
	    (hash-table-set! ht (<- key-gen) (<- value-gen)))))))))

(define (gen-record ctor . slot-gens)
  (generator (apply ctor (map <- slot-gens))))

(define (gen-transform transformer gen)
  (generator
   (transformer (<- gen))))
