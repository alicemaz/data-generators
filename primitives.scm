(use lazy-seq random-bsd srfi-1 srfi-69)

(define default-fixnum-max    (make-parameter most-positive-fixnum))

(define (show-gen gen #!optional (amount 5))
  (lazy-seq->list (lazy-take amount gen)))

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
  (make-restriction most-negative-fixnum x))

(define (at-least y)
  (make-restriction y most-positive-fixnum))

(define (between x y)
  (make-restriction x y))

(define (exactly x)
  (make-restriction x x))

(define default-restriction (make-parameter (at-most 10)))

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
(define (gen-fixnum #!key (restrict (default-restriction)))
  (let ((lower (lower-bound restrict))
        (upper (upper-bound restrict)))
    (gen-infinite
     (lambda ()
       (random-int lower upper)))))

;; infinite sequence of booleans
(define (gen-bool #!optional (val '()))
  (gen-infinite
   (lambda ()
     (if (null? val)
         (= ( random 2) 1)
         val))))



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
(define (gen-compounds seq combine #!key (restrict (default-restriction)))
  (let loop ((sample (lazy-take/random seq restrict)) (seq seq))
    (lazy-seq
     (let ((new-seq (lazy-drop (lazy-length sample) seq)))
       (cons (combine sample) (loop (lazy-take/random new-seq restrict) new-seq))))))

(define (gen-list seq . rest)
   (apply gen-compounds seq lazy-seq->list rest))

(define (gen-vector seq . rest)
  (apply gen-compounds seq (compose list->vector lazy-seq->list) rest))

(define (gen-alist key-seq value-seq . rest)
  (apply gen-compounds  (lazy-map cons key-seq value-seq) lazy-seq->list rest))

(define (gen-hash-table key-seq value-seq  #!rest r #!key (test equal?))
  (lazy-map (lambda (x) (alist->hash-table x test)) (apply gen-alist key-seq value-seq r)))

; ;; an infinite sequence producing atoms
;; (define (gen-atom successor #!key (restrict (default-restriction)))
;;   (lazy-repeatedly
;;    (lambda ()
;;      (successor restrict))))

;; (define (gen-bool/true)
;;   (gen-atom (lambda _ #t)))

;; (define (gen-bool/false)
;;   (gen-atom (lambda _ #f)))

;; (define (gen-bool/random)
;;   (gen-atom  (lambda (r) (= (random 2) 1))))



;; (define (gen-fixnum)
;;   (gen-range most-negative-fixnum most-positive-fixnum add1))






;; (define (make-number-seq min max filter succ)
;;   (let ((seq (lazy-filter filter (lazy-iterate succ min))))
;;     (if max
;;         (lazy-take (+ 1 (- max min)) seq)
;;         seq)))

;; (define (negate x) (* x -1))

;; (define (random-int min max)
;;   (+ min (random (+ 1 (- max min)))))

;; (define (gen-fixnum/sequential #!key (step 1) (min 0) (max #f) (filter (constantly #t)) (after identity))
;;   (make-number-seq min max filter (lambda (c) (after (+ c step)))))

;; (define (gen-fixnum/random #!key (min 0) (max 39085789345) (filter (constantly #t)) (after identity))
;;   (let ((next (lambda _ (after (random-int min max)))))
;;     (make-number-seq (next) max filter next)))


;; ;; boolean
;; (define (gen-bool/random)
;;   (lazy-repeatedly (lambda () (= (random 2) 1))))

;; ;; char
;; (define (gen-char/random #!key (min 0) (max 128))
;;   (lazy-repeatedly
;;    (lambda ()
;;      (integer->char (random-int min max)))))

;; (define (gen-char/sequential #!key (step 1) (min 0) (max 128))
;;   (lazy-map
;;    integer->char
;;    (gen-fixnum/sequential step: step min: min max: max)))

;; ;; strings
;; (define (gen-string/sequential #!key (length #f) (max 10) (char-min 32) (char-max 126))
;;   (lazy-repeatedly
;;    (lambda ()
;;      (list->string
;;       (lazy-seq->list
;;        (lazy-take
;;         (or length (and max (random max)))
;;         (gen-char/sequential min: char-min max: char-max)))))))

;; (define (gen-string/random #!key (max 10) (length #f) (char-min 32) (char-max 126))
;;   (lazy-repeatedly
;;    (lambda ()
;;      (list->string
;;       (lazy-seq->list
;;        (lazy-take
;;         (or length (and max (random max)))
;;         (gen-char/random min: char-min max: char-max)))))))

;; (define (gen-from-samples/sequential sample)
;;   (lazy-cycle
;;    (list->lazy-seq sample)))

;; (define (gen-from-samples/random sample)
;;   (let ((len (length sample)))
;;     (lazy-repeatedly
;;      (lambda ()
;;        (list-ref sample (random len))))))

;; ;; compound generator restrictions
;; ;; these are used to constrain the length of the generated compounds








;; (define (gen-vector seq . rest)
;;   (apply gen-sequence seq (o list->vector lazy-seq->list) rest))

;; (define (gen-alist key-seq value-seq . rest)
;;   (apply gen-sequence  (lazy-map cons key-seq value-seq) lazy-seq->list rest))

;; (define (gen-hash-table key-seq value-seq  #!rest r #!key (test equal?))
;;   (lazy-map (lambda (x) (alist->hash-table x test)) (apply gen-alist key-seq value-seq r)))
