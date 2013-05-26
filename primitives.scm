(use lazy-seq random-bsd srfi-1 srfi-69)


;; todo
;; ponder the change of the api from
;; (gen-fixnum restrict: (between 1 2))
;; to
;; (restrict (gen-fixnum) (between 1 2))
;;
;; The latter seems a bit nicer and more generic
;; also the signature of the generators would
;; get rid of DSSSL parameters
;;

(define current-fixnum-max    (make-parameter most-positive-fixnum))
(define current-fixnum-min    (make-parameter most-negative-fixnum))

(define (show-gen gen #!optional (amount 5))
  (lazy-seq->list (lazy-take amount gen)))

(define (random-int min max)
  (+ min (random (+ 1 (- max min)))))

(define (project sel) (compose sel list))

(define (upper-bound restriction)
  (restriction (project second)))

(define (lower-bound restriction)
  (restriction (project first)))

(define ((make-restriction lower upper) selector)
  (selector lower upper))

(define (at-most x)
  (make-restriction 0 x))

(define (at-most* x)
  (make-restriction (current-fixnum-min) x))

(define (at-least y)
  (make-restriction y (current-fixnum-max)))

(define (between x y)
  (make-restriction x y))

(define (exactly x)
  (make-restriction x x))

(define current-fixnum-restriction  (make-parameter (at-most 1000)))
(define current-char-restriction    (make-parameter (between (integer->char 32) (integer->char 126))))
(define current-compound-restriction (make-parameter (at-most 10)))

;; a finite sequence of the given range
(define (gen-range restriction step)
  (let ((stop (upper-bound restriction)))
    (let loop ((current (lower-bound restriction)))
      (lazy-seq
       (if (> current stop)
           '()
           (cons current (loop (step current))))))))

(define (gen-cycle gen)
  (lazy-cycle gen))

(define (gen-infinite next)
  (lazy-repeatedly next))

;; atom generators
;; ==================

;; inifinite sequence of fixnums within the restrictions
(define (gen-fixnum #!key (restrict (current-fixnum-restriction)))
  (let ((lower (lower-bound restrict))
        (upper (upper-bound restrict)))
    (gen-infinite
     (lambda ()
       (random-int lower upper)))))

(define (gen-fixnum/seq #!key (restrict (current-fixnum-restriction)))
  (gen-cycle (gen-range restrict add1)))

(define (gen-char #!key (restrict (current-char-restriction)))
  (let ((lower (char->integer (lower-bound restrict)))
        (upper (char->integer (upper-bound restrict))))
    (gen-infinite
     (lambda ()
       (integer->char (random-int lower upper))))))

(define (gen-char/seq #!key (restrict (current-char-restriction)))
  (gen-cycle (gen-range restrict integer->char)))

;; infinite sequence of booleans
(define (gen-bool)
  (gen-infinite
   (lambda () (= ( random 2) 1))))


;; Compound generators
;;============================

;; extract items from a sequence that match
;; the length property
;; (lazy-take* seq (at-most 3))
(define (lazy-take/random seq restriction)
  (lazy-take
   (random-int
    (lower-bound restriction)
    (upper-bound restriction)) seq))

;; returns a sequence of sequences
;; the combine argument is the procedure
;; that generates the compound type from a sample of atoms
(define (gen-compounds seq combine #!key (restrict (current-compound-restriction)))
  (let loop ((sample (lazy-take/random seq restrict)) (seq seq))
    (lazy-seq
     (let ((new-seq (lazy-drop (lazy-length sample) seq)))
       (cons (combine sample) (loop (lazy-take/random new-seq restrict) new-seq))))))

(define (gen-from-sample sample)
  (let ((len (length sample)))
    (lazy-repeatedly
     (lambda ()
       (list-ref sample (random len))))))

(define (gen-from-samples/seq sample)
  (lazy-cycle
   (list->lazy-seq sample)))

(define (gen-string #!key (char-restrict (current-char-restriction)) (restrict (current-compound-restriction)))
  (gen-compounds (gen-char restrict: char-restrict) (compose list->string lazy-seq->list) restrict: restrict))

(define (gen-list seq . rest)
   (apply gen-compounds seq lazy-seq->list rest))

(define (gen-vector seq . rest)
  (apply gen-compounds seq (compose list->vector lazy-seq->list) rest))

(define (gen-alist key-seq value-seq . rest)
  (apply gen-compounds  (lazy-map cons key-seq value-seq) lazy-seq->list rest))

(define (gen-hash-table key-seq value-seq  #!rest r #!key (test equal?))
  (lazy-map (lambda (x) (alist->hash-table x test)) (apply gen-alist key-seq value-seq r)))
