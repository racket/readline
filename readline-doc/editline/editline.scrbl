#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     editline
                     editline/pread
                     editline/readline
                     racket/contract
                     ffi/unsafe/atomic
                     (except-in ffi/unsafe ->)))

@title{Editline: Terminal Interaction}


The @filepath{editline} collection is analogous to the @racketmodname[readline]
collection with the exception that it uses the libedit library rather then
GNU's Readline library.

@section{Normal Use of Editline}
@defmodule*[(editline editline/rep-start)]

See documentation for @racketmodname[readline].

@section{Interacting with the Editline-Enabled Input Port}
@defmodule[editline/pread]

See documentation for @racketmodname[readline/pread].

@section{Direct Bindings for Readline Compatibility Layer}
@defmodule[editline/readline]

See documentation for @racketmodname[readline/readline].
