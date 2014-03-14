(use json data-generators)

;; generate a json document
(define (gen-json-doc #!optional (nesting 5))
  (generator
   (with-output-to-string
     (lambda ()  (json-write (<- (gen-json-value nesting)))))))

;; this is the only tricky part
;; we need to stop the recursion at a certain level of nesting and yet
;; we want to reduce the construction of generators to a minimum
(define (gen-json-value nesting)
  (let ((scalar (gen-json-scalar)))
    (if (positive? nesting)
        (gen-sample-of (gen-json-complex (sub1 nesting)) scalar)
        scalar)))

(define (gen-json-scalar)
  (gen-sample-of (gen-json-null) (gen-json-string) (gen-json-number)))

(define (gen-json-string)
  (with-size (range 0 20)
    (gen-string-of (gen-char #\A #\z))))

(define (gen-json-null)
  (gen-constant (void)))

(define (gen-json-number)
  (gen-sample-of (gen-fixnum) (gen-real)))

(define (gen-json-complex nesting)
  (gen-sample-of (gen-json-object (sub1 nesting)) (gen-json-array (sub1 nesting))))

(define (gen-json-object nesting)
  (gen-vector-of (gen-pair-of (gen-json-string) (gen-json-value (sub1 nesting)))))

(define (gen-json-array nesting)
  (gen-list-of (gen-json-value (sub1 nesting))))

;; now let's see our documents
(with-size (range 0 20)
  (gen-for-each 10 print (gen-json-doc 10)))
