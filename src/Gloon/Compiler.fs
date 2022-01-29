[<RequireQualifiedAccess>]
module Gloon.Compiler

open Gloon

let compile s = Lexer.lex s
