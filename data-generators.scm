(module data-generators
  (%random-fixnum
   gen-current-fixnum-min
   gen-current-fixnum-max
   gen-current-default-size
   gen-int8 gen-uint8 gen-int16 gen-uint16 gen-int32 gen-uint32 gen-int64 gen-uint64
   gen-bool gen-char gen-real gen-pair-of gen-list-of gen-alist-of gen-tuple-of gen-vector-of gen-string-of
   gen-fixnum gen-hash-table-of gen-sample-of
   between at-most at-least with-size)
  (import chicken scheme)
  (use (prefix random-bsd bsd:)
       (only ports with-output-to-string)
       (only srfi-1 iota list-tabulate list-ref)
       srfi-14 srfi-69 numbers)

  (include "data-generators-impl.scm"))
