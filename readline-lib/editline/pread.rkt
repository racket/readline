#lang racket/base
(require racket/unit
         ffi/unsafe
         (only-in '#%foreign ffi-obj)
         "../private/rktpread.rkt")
(provide (except-out (all-defined-out) readline-lib))

(define readline-lib (ffi-lib "libreadline" '("5" "6" "4" "")))
(define-values/invoke-unit/infer pread@)
