;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.

(module data-generators
  (gen-current-fixnum-min
   gen-current-fixnum-max
   gen-current-default-size
   generator <- <-* gen-for-each
   gen-constant  gen-int8 gen-uint8 gen-int16 gen-uint16 gen-int32 gen-uint32 gen-int64 gen-uint64
   gen-bool gen-char gen-fixnum gen-real gen-sample gen-sample-of gen-pair-of gen-tuple-of
   gen-list-of gen-alist-of gen-vector-of gen-string-of gen-hash-table-of gen-record
   with-size range make-range size-spec->gen)
  (import chicken scheme)
  (use (prefix random-bsd bsd:)
       (only ports with-output-to-string)
       (only srfi-1 iota list-tabulate list-ref)
       srfi-14 srfi-69 numbers)

  (include "data-generators-impl.scm"))
