#lang racket/base

(require "../private/rktrl.rkt" racket/unit)
(provide (except-out (all-defined-out) libreadline-path))

(define libreadline-path "libedit")
(define-values/invoke-unit/infer readline@)
