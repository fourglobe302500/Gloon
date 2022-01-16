[<AutoOpen>]
module Gloon.Types

type Span =
  { Start: int
    Length: int }
  member t.End = t.Start + t.Length

type SyntaxNode = { Span: Span; Text: string }

type LiteralValue =
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Boolean of bool

type SyntaxToken =
  // Special Tokens
  | EOF
  | Bad of SyntaxNode
  | WhiteSpace of SyntaxNode

  // Value Tokens
  | Literal of SyntaxNode * LiteralValue
  | Identifier of SyntaxNode * string

  //Operator Tokens
  | Plus of SyntaxNode
  | Minus of SyntaxNode
  | Star of SyntaxNode
  | Slash of SyntaxNode
  | Percent of SyntaxNode
  | Equals of SyntaxNode
  | LessThan of SyntaxNode
  | GreaterThan of SyntaxNode
  | ExclamationMark of SyntaxNode
  | QuestionMark of SyntaxNode
  | Pipe of SyntaxNode
  | Ampersant of SyntaxNode
  | Dot of SyntaxNode
  | Comma of SyntaxNode
  | Quote of SyntaxNode
  | DoubleQuote of SyntaxNode

  // Delimitation Tokens
  | OpenParen of SyntaxNode
  | CloseParen of SyntaxNode
  | OpenBracket of SyntaxNode
  | CloseBracket of SyntaxNode
  | OpenSquareBracket of SyntaxNode
  | CloseSquareBracket of SyntaxNode

  //Composite Tokens
  | Power of SyntaxNode
  | NotEquals of SyntaxNode
  | LessThanOrEquals of SyntaxNode
  | GreaterThanOrEquals of SyntaxNode
  | And of SyntaxNode
  | Or of SyntaxNode
  | For of SyntaxNode
  | While of SyntaxNode
  | Until of SyntaxNode
  | Do of SyntaxNode
  | If of SyntaxNode
  | Then of SyntaxNode
  | Else of SyntaxNode
