(use test)

(use data-generators numbers)

(define (in? x ls) (not (null? (member x ls))))
(define (between? a x y) (and (>= a x) (<= a y)))

(define-syntax test-fixed-range
  (syntax-rules ()
    ((_  gen lower upper)
     (test-group (symbol->string (quote gen))
       (test-assert
        (between? (gen) lower upper))))))

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

(test-fixed-range gen-int8 -127 127)
(test-fixed-range gen-uint8 0 255)
(test-fixed-range gen-int16 -32767 32767)
(test-fixed-range gen-uint16 0 65535)
(test-fixed-range gen-int32 -2147483647 2147483647)
(test-fixed-range gen-uint32 0 4294967295)
(test-fixed-range gen-int64 -9223372036854775807 9223372036854775807)
(test-fixed-range gen-uint64 0 18446744073709551615)

(test-group "gen-real"
    (test-assert
     (between? (gen-real) 0.0 1.0))
    (test-assert
     (between? (between gen-real 1.0 2.0) 1.0 2.0))
    (test-assert
     (>= (at-least gen-real 1.0) 1.0))
    (test-assert
     (<= (at-most gen-real 2.0) 2.0)))

(test-group "gen-pair-of"
    (test-assert "produces a pair"
      (pair? (gen-pair-of gen-fixnum gen-fixnum)))
    (test-assert "car is element of expected set"
      (fixnum? (car (gen-pair-of gen-fixnum gen-fixnum))))
    (test-assert "cdr is element of expected set"
      (fixnum? (cdr (gen-pair-of gen-fixnum gen-fixnum)))))

(test-group "gen-tuple-of"
    (test "produces tuple of given size"
          3 (length (gen-tuple-of gen-fixnum gen-fixnum gen-fixnum)))
    (test "each element is element of expected set"
          #t
          (every fixnum? (gen-tuple-of gen-fixnum gen-fixnum))))

(test-group "gen-list-of"
    (test-assert "produces a list"
       (list? (gen-list-of gen-fixnum)))
    (test-assert "each element is part of expected set"
       (every fixnum? (gen-list-of gen-fixnum)))
    (test "it generates list of given length"
      3
      (with-size 3
        (length (gen-list-of gen-fixnum)))))

(test-group "gen-alist-of"
   (test-assert "produces a list"
      (list? (gen-alist-of gen-char gen-fixnum)))
   (test-assert "every key is of expected set"
      (every (compose char? car) (gen-alist-of gen-char gen-fixnum)))
   (test-assert "every value is of expected set"
      (every (compose fixnum? cdr) (gen-alist-of gen-char gen-fixnum)))
   (test "it generates alist of given length"
       4
       (with-size 4 (length (gen-alist-of gen-char gen-fixnum)))))

(test-group "gen-vector-of"
   (test-assert "produces a vector"
      (vector? (gen-vector-of gen-fixnum)))
   (test-assert "every element is element of expected set"
      (every fixnum? (vector->list (gen-vector-of gen-fixnum))))
   (test "it generates vector of given length"
      4
      (with-size 4
        (vector-length (gen-vector-of gen-fixnum)))))

(test-group "gen-string-of"
    (test-assert "produces a string"
      (string? (gen-string-of)))
    (test-assert "every element is within the given char-set"
                 (every (cut char-set-contains? char-set:graphic <>)
                        (string->list
                         (gen-string-of char-set:graphic))))
    (test "it generates a string of the given length"
       10
       (with-size 10 (string-length (gen-string-of)))))

(test-group "gen-hash-table-of"
    (test-assert "produces a hash-table"
       (hash-table? (gen-hash-table-of gen-fixnum gen-fixnum)))
    (test-assert "every key is element of expected set"
       (every fixnum? (hash-table-keys (gen-hash-table-of gen-fixnum gen-fixnum))))
    (test-assert "every value is element of expected set"
       (every fixnum? (hash-table-values (gen-hash-table-of gen-fixnum gen-fixnum))))
    (test "it generates a hash-table of given size"
      10
      (with-size 10 (hash-table-size (gen-hash-table-of gen-fixnum gen-fixnum)))))

(test-group "with-size"
    (test-assert "fixed size" (= 2 (length  (with-size 2 (gen-list-of gen-fixnum)))))
    (test-assert "range" (between? (length  (with-size (2 . 4) (gen-list-of gen-fixnum))) 2 4)))

(test-group "gen->sequence"
    (test-assert "generates a procedure" (procedure? (gen->sequence 2 (gen-list-of gen-fixnum))))
    (let ((runs 1)
          (seq (gen->sequence 3 (gen-list-of gen-fixnum))))
      (test "applies proc amount times"
            3
            (begin
              (seq (lambda (value) (set! runs (add1 runs))))
              runs))))
