(module data-generators-literals
  (range-spec)
  (import chicken scheme)
  (use data-generators)

  (define-syntax range-spec
    (syntax-rules <*> (.. ...)
      ((_ ... ?stop)
       (range (gen-current-fixnum-min) (- ?stop 1)))
      ((_ ?start ...)
       (range ?start (- (gen-current-fixnum-max) 1)))
      ((_ ?start ... ?stop)
       (range (+ ?start 1) (- ?stop 1)))
      ((_ .. ?stop)
       (range (gen-current-fixnum-min) ?stop))
      ((_ ?start ..)
       (range ?start (gen-current-fixnum-max)))
      ((_ ?start .. ?stop)
       (range ?start ?stop))
      ((_ ?start ?stop)
       (range ?start ?stop))))

  (define-constant range-literal-delimiters
    '((#\{ . #\}) (#\( . #\)) (#\[ . #\])))

  (define (read-range-literal #!optional (port (current-input-port)))
    (let* ((c (read-char port))
           (c (cond ((assq c range-literal-delimiters) => cdr)
                    (else c))))
      (read-range-literal/delim c port)))

  (define (read-range-literal/delim delim port)
    (let loop ((c (peek-char port)) (exps '()))
      (cond
       ((eof-object? c)
        (error "EOF encountered while parsing range expression"))
       ((char=? c delim)
        (read-char port) ; discard
        `(range-spec ,@(reverse exps)))
       ((char-whitespace? c)
        (read-char port) ; discard whitespace
        (loop (peek-char port) exps))
       (else
        (let ((exp (read port)))
          (loop (peek-char port) (cons exp exps)))))))

  (define (read-generator-literal parser #!optional (port (current-input-port)))
    (let* ((c (read-char port))
           (c (cond ((assq c range-literal-delimiters) => cdr)
                    (else c))))
      (read-generator-literal/delim c port)))

  (define (read-generator-literal/delim delim port)
    (let loop ((c (peek-char port)) (exps '()))
      (cond
       ((eof-object? c)
        (error "EOF encountered while parsing generator expression"))
       ((char=? c delim)
        (read-char port) ; discard
        `(gen (range-spec ,@(reverse exps))))
       ((char-whitespace? c)
        (read-char port) ; discard whitespace
        (loop (peek-char port) exps))
       (else
        (let ((exp (read port)))
          (loop (peek-char port) (cons exp exps)))))))

  (set-sharp-read-syntax! #\i read-range-literal)
  (set-sharp-read-syntax! #\g read-generator-literal)


)
