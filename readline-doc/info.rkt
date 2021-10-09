#lang info

(define collection 'multi)

(define build-deps '("scribble-lib"
                     "readline-lib"
                     "racket-doc"))
(define deps '("base"))
(define update-implies '("readline-lib"))

(define pkg-desc "documentation part of \"readline\"")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
