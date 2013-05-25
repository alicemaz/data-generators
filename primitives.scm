;; number generators

(use lazy-seq random-bsd srfi-1)

(define default-max (make-parameter 10))
(define default-length (make-parameter #f))

(define (negate x) (* x -1))

(define edge-cases-for-fixnum (make-parameter (list 0 -1 1)))

(define (make-number-seq min max filter succ)
  (let ((seq (lazy-filter filter (lazy-iterate succ min))))
    (if max
        (lazy-take (+ 1 (- max min)) seq)
        seq)))

(define (draw-samples seq length maxlength)
  (lazy-take
   (or length (and maxlength (random maxlength))) seq))

(define (random-int min max)
  (+ min (random (+ 1 (- max min)))))

(define (gen-fixnum/sequential #!key (step 1) (min 0) (max #f) (filter (constantly #t)) (after identity))
  (make-number-seq min max filter (lambda (c) (after (+ c step)))))

(define (gen-fixnum/random #!key (min 0) (max 39085789345) (filter (constantly #t)) (after identity))
  (let ((next (lambda _ (after (random-int min max)))))
    (make-number-seq (next) max filter next)))

(define (gen-fixnum/edge-case)
  (lazy-repeatedly (lambda ()
                     (list-ref
                      (edge-cases-for-fixnum)
                      (random (length (edge-cases-for-fixnum)))))))
;; boolean
(define (gen-bool)
  (lazy-repeatedly (lambda () (= (random 2) 1))))

;; char
(define (gen-char/random #!key (min 0) (max 128))
  (lazy-repeatedly
   (lambda ()
     (integer->char (random-int min max)))))

(define (gen-char/sequential #!key (step 1) (min 0) (max 128))
  (lazy-map
   integer->char
   (gen-fixnum/sequential step: step min: min max: max)))

;; strings
(define (gen-string/sequential #!key (length #f) (max 10) (char-min 32) (char-max 126))
  (lazy-repeatedly
   (lambda ()
     (list->string
      (lazy-seq->list
       (lazy-take
        (or length (and max (random max)))
        (gen-char/sequential min: char-min max: char-max)))))))

(define (gen-string/random #!key (max 10) (length #f) (char-min 32) (char-max 126))
  (lazy-repeatedly
   (lambda ()
     (list->string
      (lazy-seq->list
       (lazy-take
        (or length (and max (random max)))
        (gen-char/random min: char-min max: char-max)))))))

;; list

(define (gen-list seq #!key (max 10) (length #f))
  (let loop ((sample (draw-samples seq length max)) (seq seq))
    (lazy-seq
     (let ((new-seq (lazy-drop (lazy-length sample) seq)))
       (cons (lazy-seq->list sample) (loop (draw-samples new-seq length max) new-seq))))))

;; vector

;; hash-table
(define (gen-random-element) #t)
