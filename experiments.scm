;; For the generation of data
;; you have basically two knobs that you want to be able to adjust
;;
;; 1) the selection of the next element in the set of elements that is covered by a generator. Defaults to a random element.
;; 2) the set that is covered by a generator

;; This lirary defines an abstraction that allows you to do that in a composable manner.

;; See the following examples for now:

;; Sequence of characters:
(show (gen-char))

;; Sequence of characters in a given range:
(show (restrict (gen-char) (between #\a #\z)))

;; Sequence of characters in a given range, where each character is choosen to be the successor of the previously generated
(show (choose (restrict (gen-char) (between #\a #\z)) sequentially))

;; The same works for compound generators

;; Sequence of lists of fixnums
(show (gen-list (gen-fixnum)))

;; Sequence of lists of fixnums where each list has a random length between 0 and 5
(show (restrict (gen-list (gen-fixnum)) (between 0 5)))

;; Sequence of lists of fixnums where each list has a length that is one more than the length of the list that was generated before
;; upto a length of 5
(show (choose (restrict (gen-list (gen-fixnum)) (between 0 5)) sequentially))

;; And finally all together
(show (choose (restrict (gen-list (choose (restrict (gen-char) (between #\a #\z)) uniformly)) (at-most 5)) sequentially))

;; This way you can compose your generators
(define lower-char-gen (restrict (gen-char) (between #\a #\z)))
;;
;; Now pick uniformly from them
(show (choose lower-char-gen uniformly))
;;
;; Or sequentially
(show (choose lower-char-gen sequentially))




(use srfi-1 lazy-seq)

(define current-fixnum-max    (make-parameter most-positive-fixnum))
(define current-fixnum-min    (make-parameter most-negative-fixnum))

(define (project sel) (compose sel list))

(define (upper-bound restriction)
  (restriction (project second)))

(define (lower-bound restriction)
  (restriction (project first)))

(define ((make-restriction lower upper) selector)
  (selector lower upper))

(define (between x y)
  (make-restriction x y))

(define (at-most x)
  (between 0 x))

(define (at-most* x)
  (between (current-fixnum-min) x))

(define (at-least y)
  (between y (current-fixnum-max)))

(define (exactly x)
  (between x x))

(define (gen-range restriction successor convert)
  (let ((stop (upper-bound restriction)))
    (let loop ((current (lower-bound restriction)))
      (lazy-seq
       (if (> current stop)
           '()
           (cons (convert current) (loop (successor current))))))))

(define (uniformly restriction)
  (let ((lo (lower-bound restriction))
        (hi (upper-bound restriction)))
    (lambda _
      (+ lo (random (+ 1 (- hi lo)))))))

(define (sequentially _) add1)

(define current-default-selection   (make-parameter uniformly))
(define current-default-restriction (make-parameter (at-most 10)))

(define (show it #!optional (amount 5))
  (if (lazy-seq? it)
      (lazy-seq->list (lazy-take amount it))
      (lazy-seq->list (lazy-take amount (it)))))


(define (char->integer* maybe-char)
  (if (char? maybe-char)
      (char->integer maybe-char)
      maybe-char))

(define (restrict gen restriction)
  (lambda (#!optional ignored (s (current-default-selection)))
    (gen restriction s)))

(define (choose gen selection)
  (lambda (#!optional (r (current-default-restriction)) ignored)
    (gen r selection)))


;; primitive generators
(define (gen-char)
  (lambda (#!optional (restriction (current-default-restriction)) (selection (current-default-selection)))
    (let ((r  (make-restriction
               (char->integer* (lower-bound restriction))
               (char->integer* (upper-bound restriction)))))
      (gen-range r (selection r) integer->char))))


(define (gen-fixnum)
  (lambda (#!optional (restriction (current-default-restriction)) (selection (current-default-selection)))
    (gen-range restriction (selection restriction) identity)))

(define (gen-bool)
  (lambda (#!optional (restriction (current-default-restriction)) (selection (current-default-selection)))
    (let ((r (make-restriction  0 1)))
      (gen-range r (selection r) (lambda (x) (= x 1) )))))

;; compounds
(define current-compound-restriction (make-parameter (at-most 5)))

(define (grab-sample seq sel restriction)
  (lazy-take ((sel restriction)) seq))

(define (gen->seq gen)
  (cond
   ((lazy-seq? gen) gen)
   ((procedure? gen) (gen->seq (gen)))
   (else (error "invalid generator"))))

(define (gen-compound gen combine)
  (lambda (#!optional (restriction (current-compound-restriction)) (selection uniformly))
    (let ((seq (gen->seq gen))
          (sel (selection restriction)))
      (let loop ((slice-size (sel (lower-bound restriction))) (seq seq))
        (let ((subseq (lazy-take slice-size seq)))
          (lazy-seq
           (let ((new-seq (lazy-drop slice-size seq)))
             (cons (combine subseq) (loop (sel slice-size) new-seq)))))))))

;; with the above you can do

;; (choose (gen-list (gen-char) sequentially))
;; to generate lists starting with length 0 up to default-restriction's upper bound with the generator given

;; aslo (choose (gen-list (gen-char) uniformly)) generates lists of random length

(define (gen-list gen)
  (gen-compound gen lazy-seq->list))
