#lang racket/base

;; support for bouncing/matching parens, with some inspiration from
;; Guile's readline support

(require "rktrl.rkt")

(provide match-paren-timeout
         install-match-paren-bindings!)
;; avoid a hard dependency on the lexer
(define lexer (dynamic-require 'syntax-color/racket-lexer 'racket-lexer
                               (λ () #f)))

;; timeout in milliseconds
(define match-paren-timeout (make-parameter 500))

;; int? int? -> void?
;; Matches parentheses in the buffer and flashes the current pair when a
;; new closing paren is typed. Ignores the first argument and the second
;; argument should be the key passed from readline.
(define (match-parens _ char)
  (define cur-point (readline-point))
  (readline-insert-text (make-string 1 (integer->char char)))
  (when (match-paren-timeout)
    (define new-point (find-match cur-point char))
    (when new-point
      (readline-point new-point)
      (readline-redisplay #:force? #f)
      (sleep (/ (match-paren-timeout) 1000))
      ;; move to after the newly inserted character
      (readline-point (add1 cur-point)))))

;; exact-integer? byte? -> (or/c #f exact-integer?)
;; Find the index in the readline buffer of the matching paren or
;; #f if it does not exist.
(define (find-match point char)
  (define input (buffer->input-port point))
  (define target-sym (byte->symbol char))
  (let loop ([stack null] [last-match #f])
    (define-values (match type paren-kind start end)
      (lexer input))
    (cond [(eof-object? match)
           ;; check that the match, if it exists, is actually a match
           ;; for the new inserted character (by position in the buffer)
           (and last-match
                (eq? target-sym (car (cadr last-match)))
                ;; the lexer is 1-indexed, so subtract for 0-index
                (= point (sub1 (cadr (cadr last-match))))
                (sub1 (cadr (car last-match))))]
          [(eq? type 'parenthesis)
           ;; matching pairs are removed from the stack, but remembered
           ;; for the end in case it's the new character and its match
           (if (and (not (null? stack))
                    (matching-paren? (caar stack) paren-kind))
               (loop (cdr stack)
                     (list (car stack) `(,paren-kind ,start)))
               (loop (cons `(,paren-kind ,start) stack)
                     last-match))]
          [else (loop stack last-match)])))

;; symbol? symbol? -> boolean?
;; Test if two parentheses are a matching pair
(define (matching-paren? p1 p2)
  (or (and (eq? p1 '|(|) (eq? p2 '|)|))
      (and (eq? p1 '|[|) (eq? p2 '|]|))
      (and (eq? p1 '|{|) (eq? p2 '|}|))))

;; byte? -> symbol?
;; Convert a character code for a parenthesis to a symbol
(define (byte->symbol byte)
  (string->symbol (make-string 1 (integer->char byte))))

;; exact-integer? -> input-port
;; Turn the readline buffer contents into an input port from
;; the start up to the specified point
(define (buffer->input-port point)
  (define buffer-string
    (list->string
     (for/list ([char-code (in-bytes (readline-buffer) 0 (add1 point))])
       (integer->char char-code))))
  (open-input-string buffer-string))

;; bind a startup hook to install the paren matching in the right keymap
(define close-paren-code   (char->integer #\)))
(define close-bracket-code (char->integer #\]))
(define close-brace-code   (char->integer #\}))

(define (install-match-paren-bindings!)
  (when lexer
    (readline-startup-hook
     (lambda ()
         (readline-bind-key close-paren-code   match-parens)
         (readline-bind-key close-bracket-code match-parens)
         (readline-bind-key close-brace-code   match-parens)))))
