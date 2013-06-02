;; TODO:
;; think about if we can get rid of (restrict) and just us (between gen 0 10)
;; the same for (choose gen uniformly) => (uniformly gen)
;; Thanks to sjamaan for that idea


;; For the generation of data
;; you have basically two knobs that you want to be able to adjust
;;
;; 1) the selection of the next element in the set of elements that is covered by a generator. Defaults to a random element.
;; 2) the set that is covered by a generator

;; This lirary defines an abstraction that allows you to do that in a composable manner.

;; See the following examples for now:

;; ;; Sequence of characters:
;; (show (gen-char))

;; ;; Sequence of characters in a given range:
;; (show (restrict (gen-char) (between #\a #\z)))

;; ;; Sequence of characters in a given range, where each character is choosen to be the successor of the previously generated
;; (show (choose (restrict (gen-char) (between #\a #\z)) sequentially))

;; ;; The same works for compound generators

;; ;; Sequence of lists of fixnums
;; (show (gen-list (gen-fixnum)))

;; ;; Sequence of lists of fixnums where each list has a random length between 0 and 5
;; (show (restrict (gen-list (gen-fixnum)) (between 0 5)))

;; ;; Sequence of lists of fixnums where each list has a length that is one more than the length of the list that was generated before
;; ;; upto a length of 5
;; (show (choose (restrict (gen-list (gen-fixnum)) (between 0 5)) sequentially))

;; ;; And finally all together
;; (show (choose (restrict (gen-list (choose (restrict (gen-char) (between #\a #\z)) uniformly)) (at-most 5)) sequentially))

;; ;; This way you can compose your generators
;; (define lower-char-gen (restrict (gen-char) (between #\a #\z)))
;; ;;
;; ;; Now pick uniformly from them
;; (show (choose lower-char-gen uniformly))
;; ;;
;; ;; Or sequentially
;; (show (choose lower-char-gen sequentially))

(use srfi-1 lazy-seq)

(define current-fixnum-max    (make-parameter most-positive-fixnum))
(define current-fixnum-min    (make-parameter most-negative-fixnum))

;; little helper to show generators
(define (show it #!optional (amount 5))
  (if (lazy-seq? it)
      (lazy-seq->list (lazy-take amount it))
      (lazy-seq->list (lazy-take amount (it)))))

;; restriction
(define make-restriction cons)
(define lower-bound car)
(define upper-bound cdr)

(define current-default-restriction  (make-parameter  (make-restriction 0 20)))
(define current-compound-restriction (make-parameter  (make-restriction 0 5)))

;; restriction combinators
(define (between gen lower upper)
  (lambda (#!optional _ (s (current-default-selection)))
    (gen (make-restriction lower upper) s)))

(define (at-most g x)
  (between g 0 x))

(define (at-most* g x)
  (between g (current-fixnum-min) x))

(define (at-least g y)
  (between g y (current-fixnum-max)))

(define (exactly g x)
  (between g x x))

(define (random-successor r)
  (let ((lo (lower-bound r))
        (hi (upper-bound r)))
    (lambda _
      (+ lo (random (+ 1 (- hi lo)))))))

(define current-default-selection (make-parameter random-successor))

;; selection combinators
(define (uniformly gen)
  (lambda (#!optional (r (current-default-restriction)) (s (random-successor r)))
    (gen r s)))

(define (sequentially gen)
  (lambda (#!optional (r (current-default-restriction)) ignored)
    (gen r add1)))

;; this is the most primitive generator which serves as the basis for most of the other generators
(define (gen-range restriction successor convert)
  (let ((stop (upper-bound restriction)))
    (let loop ((current (lower-bound restriction)))
      (lazy-seq
       (if (> current stop)
           '()
           (cons (convert current) (loop (successor current))))))))

;; primitive generators
(define (gen-char)
  (define (char->integer* maybe-char)
    (if (char? maybe-char)
        (char->integer maybe-char)
        maybe-char))

  (lambda (#!optional (restriction (current-default-restriction)) (selection ((current-default-selection) restriction)))
    (let ((r  (make-restriction
               (char->integer* (lower-bound restriction))
               (char->integer* (upper-bound restriction)))))
      (gen-range r selection integer->char))))


(define (gen-fixnum)
  (lambda (#!optional (restriction (current-default-restriction)) (selection ((current-default-selection) restriction)))
    (gen-range restriction selection identity)))

(define (gen-bool)
  (lambda (#!optional (restriction (current-default-restriction)) (selection ((current-default-selection) restriction)))
    (let ((r (make-restriction 0 1)))
      (gen-range r selection (lambda (x) (= x 1)) ))))

;; compounds
(define (grab-sample seq sel restriction)
  (lazy-take ((sel restriction)) seq))

(define (gen->seq gen)
  (cond
   ((lazy-seq? gen) gen)
   ((procedure? gen) (gen->seq (gen)))
   (else (error "invalid generator"))))

(define (gen-compound gen combine)
  (lambda (#!optional (restriction (current-compound-restriction)) (selection random-successor))
    (let ((seq (gen->seq gen))
          (sel (selection restriction)))
      (let loop ((slice-size (sel (lower-bound restriction))) (seq seq))
        (let ((subseq (lazy-take slice-size seq)))
          (lazy-seq
           (let ((new-seq (lazy-drop slice-size seq)))
             (cons (combine subseq) (loop (sel slice-size) new-seq)))))))))

(define (gen-list gen)
  (gen-compound gen lazy-seq->list))
