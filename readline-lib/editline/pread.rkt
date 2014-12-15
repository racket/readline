#lang racket/base
(require "../private/rktpread.rkt" racket/unit)
(provide (except-out (all-defined-out) libreadline-path))

(define libreadline-path "libedit")
(define-values/invoke-unit/infer pread@)
