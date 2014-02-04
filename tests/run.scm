(use test)

(use data-generators)

(define (in? x ls) (not (null? (member x ls))))
(define (between? a x y) (and (>= a x) (<= a y)))

(test-group "gen-bool"
    (test-assert
     (in? (gen-bool) (list #t #f))))

(test-group "gen-fixnum"
    (test-assert
      (between? (gen-fixnum) (gen-current-fixnum-min) (gen-current-fixnum-max)))
    (test-assert
     (between? (between gen-fixnum 0 4) 0 4))
    (test-assert
     (>=  (at-least gen-fixnum 2) 2))
    (test-assert
     (<=  (at-most gen-fixnum 2) 2)))

(test-group "gen-real"
    (test-assert
     (between? (gen-real) 0.0 1.0))
    (test-assert
     (between? (between gen-real 1.0 2.0) 1.0 2.0))
    (test-assert
     (>= (at-least gen-real 1.0) 1.0))
    (test-assert
     (<= (at-most gen-real 2.0) 2.0)))
