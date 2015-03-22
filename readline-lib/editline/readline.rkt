#lang racket/base
(require racket/unit
         ffi/unsafe
         (only-in '#%foreign ffi-obj)
         "../private/rktrl.rkt")
(provide (except-out (all-defined-out) readline-lib))

(define readline-lib (ffi-lib "libedit" '("3" "2" "")))
(define-values/invoke-unit/infer readline@)
