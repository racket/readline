#lang racket/base

(require "../private/rktrl.rkt" racket/unit)
(provide (all-defined-out))

(define libreadline-path "libedit")
(define-values/invoke-unit/infer readline@)
