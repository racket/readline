#lang racket/base
(require "../private/rktpread.rkt" racket/unit)
(provide (all-defined-out))

(define libreadline-path "libedit")
(define-values/invoke-unit/infer pread@)
