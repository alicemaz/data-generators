
(use test)

(use data-generators numbers)

(define (in? x ls) (not (null? (member x ls))))
(define (between? a x y) (and (>= a x) (<= a y)))

(test-begin "data-generators")

(test-group "gen-for-each"
    (let ((runs 1))
      (test "applies proc amount times"
            3
            (begin
              (gen-for-each 3 (lambda (value idx) (set! runs (add1 runs))) (gen-list-of (gen-fixnum)))
              runs))))

(test-group "<-"
            (test "it takes one element from gen"
                  3
                  (<- (gen-sample (list 3))))

            (test "it takes n elements from gen"
                  '(3 3 3)
                  (<- 3 (gen-sample (list 3)))))

(test-group "gen"
            (test-assert
             (between? (<- (gen 3 4)) 3 4))
            (test-assert
             (between? (<- (gen #f 4)) (gen-current-fixnum-min) 4))
            (test-assert
             (between? (<- (gen 3 #f)) 3 (gen-current-fixnum-max)))
            (test-assert
             (char? (<- (gen #\a #\z))))
            (test-assert
             (char-set-contains? (char-set #\a #\b #\c) (<- (gen #\a #\c)))))


(test-group "gen-constant"
            (test "it constantly returns the given value"
                  42
                  (<- (gen-constant 42))))

(test-group "gen-bool"
  (test-assert
   (in? (<- (gen-bool)) (list #t #f))))

(test-group "gen-fixnum"
    (test-assert
     (between? (<- (gen-fixnum)) (gen-current-fixnum-min) (gen-current-fixnum-max)))
    (test-assert
     (between? (<- (gen-fixnum 2 4)) 2 4))
    (test-assert
     (between? (<- (gen-fixnum (range 2 4))) 2 4))
    (test-error "lower bound <= upper bound"
     (gen-fixnum 4 2)))

(test-group "gen-even-fixnum"
           (test-assert
            (every even? (<- 100 (gen-even-fixnum))))
           (test-assert
            (between? (<- (gen-even-fixnum)) (gen-current-fixnum-min) (gen-current-fixnum-max)))
           (test-assert
            (between? (<- (gen-even-fixnum 2 4)) 2 4))
           (test-assert
            (between? (<- (gen-even-fixnum (range 2 4))) 2 4))
           (test-error "lower bound <= upper bound"
                       (gen-even-fixnum 4 2)))

(test-group "gen-odd-fixnum"
           (test-assert
            (every odd? (<- 100 (gen-odd-fixnum))))
           (test-assert
            (between? (<- (gen-odd-fixnum)) (gen-current-fixnum-min) (gen-current-fixnum-max)))
           (test-assert
            (between? (<- (gen-odd-fixnum 2 4)) 2 4))
           (test-assert
            (between? (<- (gen-odd-fixnum (range 2 4))) 2 4))
           (test-error "lower bound <= upper bound"
                       (gen-odd-fixnum 4 2)))


(define-syntax test-fixed-range
  (syntax-rules ()
    ((_  gen lower upper)
     (test-group (symbol->string (quote gen))
       (test-assert
        (between? (<- (gen)) lower upper))))))

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
     (between? (<- (gen-real)) 0.0 1.0))
    (test-assert
     (between? (<- (gen-real 1.0 2.0)) 1.0 2.0))
    (test-assert
     (between? (<- (gen-real (range 1.0 2.0))) 1.0 2.0))
    (test-error "lower bound <= upper bound"
     (gen-real 2.0 1.0)))

(test-group "gen-char"
    (test-assert
     (char-set-contains? char-set:graphic (<- (gen-char))))
    (test-assert
     (char-set-contains? char-set:digit (<- (gen-char char-set:digit))))
    (test-assert
     (char-set-contains? (char-set #\a #\b #\c) (<- (gen-char #\a #\c))))
    (test-assert
     (char-set-contains? (char-set #\a #\b #\c) (<- (gen-char (range #\a #\c)))))
    (test-error "lower bound <= upper bound"
     (gen-char #\z #\a)))

(test-group "gen-sample"
    (test-assert (between? (<- (gen-sample (iota 10))) 0 9))
    (test-assert (in? (<- (gen-sample (list #\a #\b #\c))) (list #\a #\b #\c))))


(test-group "gen-sample-of"
            (test-assert ((lambda (e) (or (fixnum? e) (char? e)))
			  (<- (gen-sample-of (gen-fixnum) (gen-char))))))

(test-group "gen-pair-of"
    (test-assert "produces a pair"
                 (pair? (<- (gen-pair-of (gen-fixnum) (gen-fixnum)))))
    (test-assert "car is element of expected set"
                 (fixnum? (car (<- (gen-pair-of (gen-fixnum) (gen-fixnum))))))
    (test-assert "cdr is element of expected set"
                 (fixnum? (cdr (<- (gen-pair-of (gen-fixnum) (gen-fixnum)))))))

(test-group "gen-values-of"
            (test-assert "produces values"
                  (receive (a b c) (<- (gen-values-of (gen-fixnum) (gen-fixnum) (gen-char)))
                    (and (fixnum? a) (fixnum? b) (char? c)))))

(test-group "gen-tuple-of"
    (test "produces tuple of given size"
          3 (length (<- (gen-tuple-of (gen-fixnum) (gen-fixnum) (gen-fixnum)))))
    (test "each element is element of expected set"
          #t
          (every fixnum? (<- (gen-tuple-of (gen-fixnum) (gen-fixnum))))))


(define-syntax test-size-spec-support
  (syntax-rules ()
    ((_ ?length (?gen ?args ...))
     (let ((length-matches (lambda (from to ls)
			     (every (lambda (e) (between? e from to)) (map ?length ls)))))
       (test-group "supports size-spec"
		   (test-assert "range"
				(length-matches 0 10  (<- 10 (?gen ?args ... (range 0 10)))))
		   (test-assert "fixed size"
				(length-matches 10 10 (<- 10 (?gen ?args ... 10))))
		   (test-assert "generator"
				(length-matches 0 10  (<- 10 (?gen ?args ... (gen-fixnum 0 10)))))
		   (test-assert "it generates a list with different lengths"
				(let ((ls (map ?length (<- 10 (?gen ?args ... (gen-fixnum 0 100))))))
				  (not (every (lambda (e) (= e (car ls))) ls)))))))))


(test-group "gen-list-of"
    (test-assert "produces a list"
                 (list? (<- (gen-list-of (gen-fixnum)))))
    (test-assert "each element is part of expected set"
                 (every fixnum? (<- (gen-list-of (gen-fixnum)))))
    (test-assert "accepts ranges"
		 (between? (length (<- (gen-list-of (gen-fixnum) (range 1 10)))) 1 10))

    (test-size-spec-support length (gen-list-of (gen-fixnum))))


(test-group "gen-alist-of"
   (test-assert "produces a list"
                (list? (<- (gen-alist-of (gen-char) (gen-fixnum)))))
   (test-assert "every key is of expected set"
                (every (compose char? car) (<- (gen-alist-of (gen-char) (gen-fixnum)))))
   (test-assert "every value is of expected set"
                (every (compose fixnum? cdr) (<- (gen-alist-of (gen-char) (gen-fixnum)))))
    (test-size-spec-support length (gen-alist-of (gen-fixnum) (gen-fixnum))))

(test-group "gen-vector-of"
   (test-assert "produces a vector"
                (vector? (<- (gen-vector-of (gen-fixnum)))))
   (test-assert "every element is element of expected set"
                (every fixnum?  (vector->list (<- (gen-vector-of (gen-fixnum))))))
   (test-size-spec-support vector-length (gen-vector-of (gen-fixnum))))

(test-group "gen-string-of"
    (test-assert "produces a string"
                 (string? (<- (gen-string-of (gen-char)))))
    (test-assert "every element is within the given char-set"
                 (every (cut char-set-contains? char-set:graphic <>)
                        (string->list
                         (<- (gen-string-of (gen-char char-set:graphic))))))
    (test-size-spec-support string-length (gen-string-of (gen-char))))

(test-group "gen-hash-table-of"
    (test-assert "produces a hash-table"
                 (hash-table? (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum)))))
    (test-assert "every key is element of expected set"
                 (every fixnum? (hash-table-keys (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum))))))
    (test-assert "every value is element of expected set"
                 (every fixnum? (hash-table-values (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum))))))
    (test-size-spec-support (o length hash-table-keys) (gen-hash-table-of (gen-fixnum) (gen-fixnum))))


(define-record test-record x y)

(test-group "gen-record"
            (let ((record (<- (gen-record make-test-record (gen-fixnum) (gen-fixnum)))))
              (test-assert
               (test-record? record))
              (test-assert
               (fixnum? (test-record-x record)))
              (test-assert
               (fixnum? (test-record-y record)))))

(test-group "with-size"
            (test-assert "fixed size" (= 2 (length  (with-size 2 (<- (gen-list-of (gen-fixnum)))))))
            (test-assert "range" (between? (length  (with-size (range 2 4) (<- (gen-list-of (gen-fixnum))))) 2 4)))



;; (use data-generators-literals)

;; (test-group "range syntax"
;; 	    (test-group "inclusive"
;; 			(test "neg/inf to upper"
;; 			      (range (gen-current-fixnum-min)  5)
;; 			      #i[.. 5])
;; 			(test "lower to pos/inf"
;; 			      (range 3 (gen-current-fixnum-max))
;; 			      #i[3 ..])
;; 			(test "lower .. upper"
;; 			      (range 0 10)
;; 			      #i[0 .. 10]))
;;             (test-group "exclusive"
;; 			(test "neg/inf to upper"
;; 			      (range (add1 (gen-current-fixnum-min)) 5)
;; 			      #i[... 5])
;; 			(test "lower to pos/inf"
;; 			      (range 3 (sub1 (gen-current-fixnum-max)))
;; 			      #i[3 ...])
;; 			(test "lower ... upper"
;; 			      (range 1 9)
;; 			      #i[0 ... 10])))

;; (test-group "generator syntax"
;;             (test-assert (fixnum? (<- #g[1.0 .. 2.0]))))

            ;; (test-assert "#g[1.0 .. 4.0]"
            ;;              (flonum? (<- #g[1.0 .. 4.0])))
            ;; (test-assert "#g[#\a .. #\z]"
            ;;              (char? (<- #g[#\a .. #\z])))
            ;; (test-error "unsupported generator"
            ;;             #g[#t .. #f]))

(test-end "data-generators")

(test-exit)
