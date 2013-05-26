(use test)

(load "primitives.scm")

(define (in? x ls) (not (null? (member x ls))))
(define (between? a x y) (and (>= a x) (<= a y)))
(define result lazy-head)

(test-group "gen-bool"
    (test-assert
     (in? (result (gen-bool)) (list #t #f))))

(test-group "gen-fixnum"
    (test-assert
      (between? (result (gen-fixnum)) 0 (current-fixnum-max))))
