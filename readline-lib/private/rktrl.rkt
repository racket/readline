#lang racket/base

; Provides racket bindings for a given implementation of readline.

(require ffi/unsafe (only-in '#%foreign ffi-obj) racket/unit "sig.rkt")
(provide readline@)

(define-unit readline@
  (import readline-lib^)
  (export readline^)

  ;; libncurses and/or libtermcap needed on some platforms
  (void (ffi-lib "libcurses" #:fail (lambda () #f)))
  (void (ffi-lib "libtermcap" #:fail (lambda () #f)))

  (define libreadline readline-lib)

  (unless libreadline
    (log-warning "mzrl warning: could not load libedit"))

  ;; need to capture the real input port below
  (define real-input-port (current-input-port))
  (unless (eq? 'stdin (object-name real-input-port))
    (log-warning "mzrl warning: could not capture the real input port\n"))
  (unless (terminal-port? real-input-port)
    (log-warning "mzrl warning: input port is not a terminal\n"))

  (define real-output-port (current-output-port))
  (unless (eq? 'stdout (object-name real-input-port))
    (log-warning "mzrl warning: could not capture the real output port\n"))
  (unless (terminal-port? real-input-port)
    (log-warning "mzrl warning: output port is not a terminal\n"))
  (define make-byte-string ; helper for the two types below
    (get-ffi-obj "scheme_make_byte_string" #f (_fun _pointer -> _scheme)))

  (define _bytes/eof/free ; register a finalizer on the resulting bytes
    (make-ctype _pointer
                (lambda (x) (and (not (eof-object? x)) x))
                (lambda (x)
                  (if x
                      (let ([b (make-byte-string x)])
                        (register-finalizer b (lambda (_) (free x)))
                        b)
                      eof))))

  (define _string/eof/free ; make a Scheme str from C str & free immediately
    (make-ctype _pointer
                (lambda (x) (and (not (eof-object? x)) (string->bytes/utf-8 x)))
                (lambda (x)
                  (if x
                      (let ([s (bytes->string/utf-8 (make-byte-string x))]) (free x) s)
                      eof))))

  (define readline
    (if libreadline
        (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof/free))
        (lambda (x) (display x real-output-port) (read-line real-input-port))))

  (define readline-bytes
    (if libreadline
        (get-ffi-obj "readline" libreadline (_fun _bytes -> _bytes/eof/free))
        (lambda (x) (display x real-output-port) (read-bytes-line real-input-port))))

  (define add-history
    (if libreadline
        (get-ffi-obj "add_history" libreadline (_fun _string -> _void))
        (lambda (x) (void))))

  (define add-history-bytes
    (if libreadline
        (get-ffi-obj "add_history" libreadline (_fun _bytes -> _void))
        (lambda (x) (void))))

  (define history-length
    (if libreadline
        (let ([hl (ffi-obj #"history_length" libreadline)])
          (lambda () (ptr-ref hl _int)))
        (lambda () 0)))
  (define history-base
    (if libreadline
        (let ([hb (ffi-obj #"history_base" libreadline)])
          (lambda () (ptr-ref hb _int)))
        (lambda () 0)))

  ;; The history library has this great feature: *some* function consume
  ;; an index that is relative to history_base, and *some* get a plain
  ;; offset.  Someone just had so much fun they had to share.  This
  ;; deals with this absurdity, checks the range of the index, and deals
  ;; with negative offsets.
  (define (hist-idx who idx base?)
    (let* ([len (history-length)]
           [idx (cond [(<= 0 idx (sub1 len)) idx]
                      [(<= (- len) idx -1)   (+ len idx)]
                      [else (error who "index out of history range, -~a - ~a"
                                   len (sub1 len))])])
      (if base? (+ idx (history-base)) idx)))

  ;; actually, returns a pointer to a struct with the string, but all we
  ;; care about is the string...
  (define history-get
    (if libreadline
        (get-ffi-obj "history_get" libreadline
                     (_fun (i) :: (_int = (hist-idx 'history-get i #t)) -> (_ptr o _string)))
        (lambda (x) "")))

  (define history-remove ; returns HIST_ENTRY* that history_free() frees
    (if libreadline
        (get-ffi-obj "remove_history" libreadline
                     (_fun (i) :: (_int = (hist-idx 'history-delete i #f)) -> _pointer))
        (lambda (x) 0)))

  (define history-free ; ignore histdata_t return value
    (if libreadline
        (get-ffi-obj "free_history_entry" libreadline (_fun _pointer -> _void)
                     ;; if not available, use free
                     (lambda () free))
        (lambda (x) (void))))

  (define (history-delete idx)
    (history-free (history-remove idx)))

  ;; Simple completion: use this with a (string -> (list-of string)) function
  ;; that returns the completions for a given string (can be used with other
  ;; input string types too, depending on the `type' argument).  Use #f to remove
  ;; a completion function that was previously set.
  (define set-completion-function!
    (if libreadline
        (case-lambda
          [(func) (set-completion-function! func _string)]
          [(func type)
           (if func
               (set-ffi-obj! "rl_completion_entry_function" libreadline
                             (_fun type _int -> _pointer)
                             (completion-function func))
               (set-ffi-obj! "rl_completion_entry_function" libreadline _pointer #f))])
        (lambda (x) (void))))

  (define (completion-function func)
    (let ([cur '()])
      (define (complete str state)
        (if (zero? state)
            (begin (set! cur (func str)) (complete #f 1))
            (and (pair? cur)
                 (let* ([cur (begin0 (car cur) (set! cur (cdr cur)))]
                        [cur (if (string? cur) (string->bytes/utf-8 cur) cur)])
                   (malloc (add1 (bytes-length cur)) cur 'raw)))))
      complete))

  (when libreadline
    (set-ffi-obj! "rl_readline_name" libreadline _pointer
                  (let ([s #"mzscheme"])
                    (define m (malloc (add1 (bytes-length s)) 'atomic-interior))
                    (memcpy m s (add1 (bytes-length s)))
                    m)))


  ;; We need to tell readline to pull content through our own function,
  ;; to avoid buffering issues between C and Racket, and to allow
  ;; racket threads to run while waiting for input.
  (when libreadline
    (set-ffi-obj! "rl_getc_function" libreadline (_fun _pointer -> _int)
                  (lambda (_)
                    (define next-byte (read-byte real-input-port))
                    (if (eof-object? next-byte) -1 next-byte))))


  ;; force cursor on a new line
  (define readline-newline
    (if libreadline
        (get-ffi-obj "rl_crlf" libreadline (_fun -> _void)
                     (lambda ()
                       (get-ffi-obj "rl_newline" libreadline (_fun -> _void))))
        (lambda () (void))))

  ;; force redisplay of prompt and current user input
  (define readline-redisplay
    (if libreadline
        (get-ffi-obj "rl_forced_update_display" libreadline (_fun -> _void))
        (lambda () (void)))))
