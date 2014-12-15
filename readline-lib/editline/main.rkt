#lang racket/base

(require racket/runtime-path racket/file racket/unit)

(define-runtime-path rep-start "rep-start.rkt")

(provide install-editline!)

(let ([inp (current-input-port)] [outp (current-output-port)])
  (when (and (eq? 'stdin (object-name inp)) (terminal-port? inp))
    (dynamic-require rep-start #f)))

(define readline-init-expr
  '(require editline))

(define (install-editline!)
  (define file (find-system-path 'init-file))
  (define (add! msg)
    (call-with-output-file* file #:exists 'append
      (lambda (o)
        (fprintf o "\n;; load editline support ~a\n~s\n"
                 "(added by `install-editline!')"
                 readline-init-expr)))
    (printf msg file))
  (cond [(not (file-exists? file))
         (add! "\"~a\" created and editline initialization added.\n")]
        [(with-handlers ([exn:fail?
                          (lambda (exn)
                            (error 'install-editline!
                                   "trouble reading existing ~e: ~a"
                                   file
                                   (exn-message exn)))])
           (not (member readline-init-expr (file->list file))))
         (add! "Editline initialization added to \"~a\".\n")]
        [else (printf "Editline already installed in \"~a\".\n" file)]))
