#lang racket/base

(require ffi/unsafe (only-in '#%foreign ffi-obj)
         syntax-color/racket-lexer)
(provide readline readline-bytes
         add-history add-history-bytes
         history-length history-get history-delete
         set-completion-function!
         readline-newline readline-redisplay
         match-paren-timeout)

;; libncurses and/or libtermcap needed on some platforms
(void (ffi-lib "libcurses" #:fail (lambda () #f)))
(void (ffi-lib "libtermcap" #:fail (lambda () #f)))

(define libreadline (ffi-lib "libreadline" '("5" "6" "4" "")))

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
  (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof/free)))

(define readline-bytes
  (get-ffi-obj "readline" libreadline (_fun _bytes -> _bytes/eof/free)))

(define add-history
  (get-ffi-obj "add_history" libreadline (_fun _string -> _void)))

(define add-history-bytes
  (get-ffi-obj "add_history" libreadline (_fun _bytes -> _void)))

(define history-length
  (let ([hl (ffi-obj #"history_length" libreadline)])
    (lambda () (ptr-ref hl _int))))
(define history-base
  (let ([hb (ffi-obj #"history_base" libreadline)])
    (lambda () (ptr-ref hb _int))))

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
  (get-ffi-obj "history_get" libreadline
    (_fun (i) :: (_int = (hist-idx 'history-get i #t)) -> (_ptr o _string))))

(define history-remove ; returns HIST_ENTRY* that history_free() frees
  (get-ffi-obj "remove_history" libreadline
    (_fun (i) :: (_int = (hist-idx 'history-delete i #f)) -> _pointer)))
(define history-free ; ignore histdata_t return value
  (get-ffi-obj "free_history_entry" libreadline (_fun _pointer -> _void)
               ;; if not available, use free
               (lambda () free)))

(define (history-delete idx)
  (history-free (history-remove idx)))

;; Simple completion: use this with a (string -> (list-of string)) function
;; that returns the completions for a given string (can be used with other
;; input string types too, depending on the `type' argument).  Use #f to remove
;; a completion function that was previously set.
(define set-completion-function!
  (case-lambda
    [(func) (set-completion-function! func _string)]
    [(func type)
     (if func
       (set-ffi-obj! "rl_completion_entry_function" libreadline
                     (_fun type _int -> _pointer)
                     (completion-function func))
       (set-ffi-obj! "rl_completion_entry_function" libreadline _pointer #f))]))

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

(set-ffi-obj! "rl_readline_name" libreadline _pointer
              (let ([s #"mzscheme"])
                (define m (malloc (add1 (bytes-length s)) 'atomic-interior))
                (memcpy m s (add1 (bytes-length s)))
                m))

;; need to capture the real input port below
(define real-input-port (current-input-port))
(unless (eq? 'stdin (object-name real-input-port))
  (log-warning "mzrl warning: could not capture the real input port\n"))
(unless (terminal-port? real-input-port)
  (log-warning "mzrl warning: input port is not a terminal\n"))


;; We need to tell readline to pull content through our own function,
;; to avoid buffering issues between C and Racket, and to allow
;; racket threads to run while waiting for input.
(set-ffi-obj! "rl_getc_function" libreadline (_fun _pointer -> _int)
              (lambda (_)
                (define next-byte (read-byte real-input-port))
                (if (eof-object? next-byte) -1 next-byte)))


;; force cursor on a new line
(define readline-newline
  (get-ffi-obj "rl_crlf" libreadline (_fun -> _void)
               (lambda ()
                 (get-ffi-obj "rl_newline" libreadline (_fun -> _void)))))

;; force redisplay of prompt and current user input
(define readline-redisplay
  (get-ffi-obj "rl_forced_update_display" libreadline (_fun -> _void)))

;; support for bouncing/matching parens, with some inspiration from
;; Guile's readline support

;; timeout in milliseconds
(define match-paren-timeout (make-parameter 500))

;; these internal bindings aren't for export
(define rl-buffer (ffi-obj #"rl_line_buffer" libreadline))
(define rl-point  (ffi-obj #"rl_point" libreadline))

(define rl-redisplay (get-ffi-obj #"rl_redisplay" libreadline (_fun -> _void)))
(define rl-bind-key  (get-ffi-obj #"rl_bind_key" libreadline
                                  (_fun _int (_fun _int _int -> _void)
                                        -> _void)))
(define rl-insert    (get-ffi-obj #"rl_insert" libreadline
                                  (_fun _int _int -> _void)))

;; int? int? -> void?
;; Matches parentheses in the buffer and flashes the current pair when a
;; new closing paren is typed. Ignores the first argument and the second
;; argument should be the key passed from readline.
(define (match-parens _ char)
  (define cur-point (ptr-ref rl-point _int))
  (rl-insert 1 char)
  (when (match-paren-timeout)
    (define new-point (find-match cur-point char))
    (when new-point
      (ptr-set! rl-point _int new-point)
      (rl-redisplay)
      (sleep (/ (match-paren-timeout) 1000))
      ;; move to after the newly inserted character
      (ptr-set! rl-point _int (add1 cur-point)))))

;; exact-integer? byte? -> (or/c #f exact-integer?)
;; Find the index in the readline buffer of the matching paren or
;; #f if it does not exist.
(define (find-match point char)
  (define input (buffer->input-port point))
  (define target-sym (byte->symbol char))
  (let loop ([stack null] [last-match #f])
    (define-values (match type paren-kind start end)
      (racket-lexer input))
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
    (list->string (for/list ([idx (in-range (add1 point))])
                    (integer->char (buffer-ref idx)))))
  (open-input-string buffer-string))

;; exact-integer? -> byte?
;; Dereference a byte in the readline buffer
(define (buffer-ref idx)
  (ptr-ref (ptr-ref rl-buffer _pointer) _byte idx))

;; bind a startup hook to install the paren matching in the right keymap
(define close-paren-code   (char->integer #\)))
(define close-bracket-code (char->integer #\]))
(define close-brace-code   (char->integer #\}))

(set-ffi-obj! "rl_startup_hook" libreadline (_fun -> _void)
              (lambda ()
                (rl-bind-key close-paren-code   match-parens)
                (rl-bind-key close-bracket-code match-parens)
                (rl-bind-key close-brace-code   match-parens)))
