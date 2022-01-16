[<RequireQualifiedAccess>]
module Gloon.Lexer

open System

let private (|Letter|_|) c =
  if Char.IsLetter c then
    Some <| string c
  else
    None

let private (|Number|_|) c =
  if Char.IsNumber c then
    Some(int c - int '0', string c)
  else
    None

let private (|WhiteSpace|_|) c =
  if Char.IsWhiteSpace c then
    Some <| string c
  else
    None

let private map i =
  function
  | Letter l -> Identifier(SyntaxNode.Basic l (i + 1))
  | Number (n, t) -> Literal(SyntaxNode.Basic t (i + 1), Int n)
  | WhiteSpace w -> WhiteSpace <| SyntaxNode.Basic w (i + 1)
  | '\n' -> WhiteSpace <| SyntaxNode.Basic "\n" (i + 1)
  | '\r' -> WhiteSpace <| SyntaxNode.Basic "\r" (i + 1)
  | '.' -> Dot <| SyntaxNode.Basic "." (i + 1)
  | '+' -> Plus <| SyntaxNode.Basic "+" (i + 1)
  | '-' -> Minus <| SyntaxNode.Basic "-" (i + 1)
  | '*' -> Star <| SyntaxNode.Basic "*" (i + 1)
  | '/' -> Slash <| SyntaxNode.Basic "/" (i + 1)
  | '\\' -> BackSlash <| SyntaxNode.Basic "\\" (i + 1)
  | '%' -> Percent <| SyntaxNode.Basic "%" (i + 1)
  | '=' -> Equals <| SyntaxNode.Basic "=" (i + 1)
  | '<' -> LessThan <| SyntaxNode.Basic "<" (i + 1)
  | '>' -> GreaterThan <| SyntaxNode.Basic ">" (i + 1)
  | '!' -> ExclamationMark <| SyntaxNode.Basic "!" (i + 1)
  | '?' -> QuestionMark <| SyntaxNode.Basic "?" (i + 1)
  | '|' -> Pipe <| SyntaxNode.Basic "|" (i + 1)
  | '&' -> Ampersant <| SyntaxNode.Basic "&" (i + 1)
  | '\'' -> Quote <| SyntaxNode.Basic "'" (i + 1)
  | '"' -> DoubleQuote <| SyntaxNode.Basic "\"" (i + 1)
  | c -> Bad <| SyntaxNode.Basic(c.ToString()) (i + 1)


let lex (s: string) = List.ofSeq s |> List.mapi map
