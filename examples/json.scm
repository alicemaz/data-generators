(use json data-generators)

;; generate a json document
(define (gen-json-doc #!optional (nesting 5))
  (generator
   (with-output-to-string
     (lambda ()
       (json-write (<- (gen-json-value nesting)))))))

(define (gen-json-value nesting)
  (generator
   (if (positive? nesting)
       (<- (gen-sample-of (gen-json-string) (gen-json-array nesting) (gen-json-null) (gen-json-number) (gen-json-object nesting)))
       (<- (gen-sample-of (gen-json-string) (gen-json-null) (gen-json-number))))))

(define (gen-json-string)
  (with-size (0 . 20)
    (gen-string-of (gen-char char-set:letter+digit))))

(define (gen-json-object nesting)
  (with-size (0 . 5)
    (gen-vector-of (gen-pair-of (gen-json-string) (gen-json-value (sub1 nesting))))))

(define (gen-json-null)
  (gen-constant (void)))

(define (gen-json-number)
  (gen-sample-of (gen-fixnum) (gen-real)))

(define (gen-json-array nesting)
  (with-size (0 . 5)
    (gen-list-of (gen-json-value (sub1 nesting)))))

(gen-for-each 10 print (gen-json-doc))
