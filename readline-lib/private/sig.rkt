#lang racket/base

;; Provides signatures for the readline and pread units.

(require racket/unit)

(provide (all-defined-out))

(define-signature readline^
  (readline
   readline-bytes
   add-history
   add-history-bytes
   history-length
   history-get
   history-delete
   set-completion-function!
   readline-newline
   readline-redisplay))

(define-signature readline-lib^
  (readline-lib))

(define-signature pread^
  (current-prompt
   max-history
   keep-duplicates
   keep-blanks
   readline-prompt
   readline-input
   read-cmdline-syntax))

