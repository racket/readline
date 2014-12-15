#lang racket/base
(require racket/unit "../private/rktrl.rkt")
(provide (except-out (all-defined-out) libreadline-path))

(define libreadline-path "libreadline")
(define-values/invoke-unit/infer readline@)
