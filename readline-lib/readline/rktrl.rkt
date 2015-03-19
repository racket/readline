#lang racket/base

(require ffi/unsafe (only-in '#%foreign ffi-obj))
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

(define open-paren-code    (char->integer #\())
(define open-bracket-code  (char->integer #\[))
(define open-brace-code    (char->integer #\{))
(define close-paren-code   (char->integer #\)))
(define close-bracket-code (char->integer #\]))
(define close-brace-code   (char->integer #\}))

;; int? int? -> void?
;; Matches parentheses in the buffer and flashes the current pair when a
;; new closing paren is typed. Ignores the first argument and the second
;; argument should be the key passed from readline.
(define (match-parens _ char)
  (define cur-point (ptr-ref rl-point _int))
  (rl-insert 1 char)
  (when (and (match-paren-timeout)
             (or (= char close-paren-code)
                 (= char close-bracket-code)
                 (= char close-brace-code))
             ;; don't try to match if the user typed #\)
             (not (and (>= cur-point 2)
                       (= (buffer-ref (- cur-point 1))
                          (char->integer #\\))
                       (= (buffer-ref (- cur-point 2))
                          (char->integer #\#)))))
    (define new-point (find-match cur-point char))
    (when new-point
      (ptr-set! rl-point _int new-point)
      (rl-redisplay)
      (sleep (/ (match-paren-timeout) 1000))
      ;; move to after the newly inserted character
      (ptr-set! rl-point _int (add1 cur-point)))))

;; exact-integer? byte? -> exact-integer?
;; Find the index in the readline buffer of the matching paren or
;; #f if it does not exist.
(define (find-match point char)
  (let loop ([point (sub1 point)] ; start before new character
             [close-parens 0])
    (if (= point -1) ; don't flash after going off the end
        #f
        (let ([point-char (buffer-ref point)])
          (cond ;; skip strings and byte strings
                [(= point-char (char->integer #\"))
                 (loop (skip-string-or-bytes point) close-parens)]
                ;; skip literal characters
                [(and (>= point 2)
                      (= (buffer-ref (- point 1)) (char->integer #\\))
                      (= (buffer-ref (- point 2)) (char->integer #\#)))
                 (loop (- point 3) close-parens)]
                ;; skip '|| symbols
                [(and (>= point 2)
                      (= (buffer-ref (- point 1)) (char->integer #\|))
                      (= (buffer-ref (- point 2)) (char->integer #\')))
                 (loop (- point 3) close-parens)]
                ;; track and skip matching pairs
                [(= point-char char)
                 (loop (sub1 point) (add1 close-parens))]
                [(or (and (= char close-paren-code)
                          (= point-char open-paren-code))
                     (and (= char close-bracket-code)
                          (= point-char open-bracket-code))
                     (and (= char close-brace-code)
                          (= point-char open-brace-code)))
                 (if (zero? close-parens)
                     point
                     (loop (sub1 point) (sub1 close-parens)))]
                [else (loop (sub1 point) close-parens)])))))

;; exact-integer? -> exact-integer?
;; Skip a Racket string of byte string in the readline buffer
(define (skip-string-or-bytes point)
  (let loop ([point (sub1 point)])
    (cond [(< point 0) ; went off end, no string found
           point]
          [(= (buffer-ref point) (char->integer #\"))
           (sub1 point)]
          [else (loop (sub1 point))])))

;; exact-integer? -> byte?
;; Dereference a byte in the readline buffer
(define (buffer-ref idx)
  (ptr-ref (ptr-ref rl-buffer _pointer) _byte idx))

;; bind a startup hook to install the paren matching in the right keymap
(set-ffi-obj! "rl_startup_hook" libreadline (_fun -> _void)
              (lambda ()
                (rl-bind-key close-paren-code   match-parens)
                (rl-bind-key close-bracket-code match-parens)
                (rl-bind-key close-brace-code   match-parens)))
