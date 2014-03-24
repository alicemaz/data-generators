(module data-generators-net
  (gen-ipv4-address)
  (import chicken scheme)
  (require-library data-generators srfi-13)
  (import (only srfi-13 string-join))
  (import (only data-generators gen-list-of gen-uint8 gen-transform))

  (define (gen-ipv4-address)
    (let ((octet-gen (gen-transform number->string (gen-uint8))))
      (gen-transform (cut string-join <> ".") (gen-list-of octet-gen 4)))))
