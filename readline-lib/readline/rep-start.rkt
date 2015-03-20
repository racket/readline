;; This module initializes readline unconditionally, "rep.rkt" uses it if we're
;; using a `terminal-port?' for input.

#lang racket/base

(require "pread.rkt"
         "match-parens.rkt")

;; Change the input port and readline-prompt hook
(current-input-port readline-input)
(current-prompt-read read-cmdline-syntax)

;; Add parenthesis matching
(install-match-paren-bindings!)
