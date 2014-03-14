(module data-generators-literals
  (range-spec)
  (import chicken scheme)
  (use data-generators)

  (define-syntax range-spec
    (syntax-rules <*> (.. ...)
      ((_ ... ?stop)
       (range (add1 (gen-current-fixnum-min)) ?stop))
      ((_ ?start ...)
       (range ?start (sub1 (gen-current-fixnum-max))))
      ((_ ?start ... ?stop)
       (range (add1 ?start) (sub1 ?stop)))
      ((_ .. ?stop)
       (range (gen-current-fixnum-min) ?stop))
      ((_ ?start ..)
       (range ?start (gen-current-fixnum-max)))
      ((_ ?start .. ?stop)
       (range ?start ?stop))
      ((_ ?start ?stop)
       (range ?start ?stop))))

  (define-constant literal-delimiters
    '((#\{ . #\}) (#\( . #\)) (#\[ . #\])))

  (define (read-literal finish port)
    (let* ((c (read-char port))
           (c (cond ((assq c literal-delimiters) => cdr)
                    (else c))))
      (finish (read-literal/delim c port))))

  (define (read-literal/delim delim port)
    (let loop ((c (peek-char port)) (exps '()))
      (cond
       ((eof-object? c)
        (error "EOF encountered while parsing range expression"))
       ((char=? c delim)
        (read-char port) ; discard
        (reverse exps))
       ((char-whitespace? c)
        (read-char port) ; discard whitespace
        (loop (peek-char port) exps))
       (else
        (let ((exp (read port)))
          (loop (peek-char port) (cons exp exps)))))))

  (define (read-range-literal #!optional (port (current-input-port)))
    (read-literal (lambda (e) `(range-spec ,@e)) port))

  (define (read-generator-literal #!optional (port (current-input-port)))
    (read-literal (lambda (e) `(gen (range-spec ,@e))) port))

  (set-sharp-read-syntax! #\i read-range-literal)
  (set-sharp-read-syntax! #\g read-generator-literal)


)
