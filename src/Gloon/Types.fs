[<AutoOpen>]
module Gloon.Types

type Span =
  { Start: int
    Length: int }
  member t.End = t.Start + t.Length
  static member FromBounds s e = { Start = s; Length = e - s }

type SyntaxNode =
  { Span: Span
    Text: string }
  static member Basic t i =
    { Text = t
      Span = { Start = i; Length = t.Length } }

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
  | Identifier of SyntaxNode

  //Operator Tokens
  | Plus of SyntaxNode
  | Minus of SyntaxNode
  | Star of SyntaxNode
  | Slash of SyntaxNode
  | BackSlash of SyntaxNode
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

[<RequireQualifiedAccess>]
module SyntaxToken =
  let text =
    function
    | EOF -> ""
    | Bad node -> node.Text
    | WhiteSpace node -> node.Text
    | Literal (node, _) -> node.Text
    | Identifier node -> node.Text
    | Plus node -> node.Text
    | Minus node -> node.Text
    | Star node -> node.Text
    | Slash node -> node.Text
    | BackSlash node -> node.Text
    | Percent node -> node.Text
    | Equals node -> node.Text
    | LessThan node -> node.Text
    | GreaterThan node -> node.Text
    | ExclamationMark node -> node.Text
    | QuestionMark node -> node.Text
    | Pipe node -> node.Text
    | Ampersant node -> node.Text
    | Dot node -> node.Text
    | Comma node -> node.Text
    | Quote node -> node.Text
    | DoubleQuote node -> node.Text
    | OpenParen node -> node.Text
    | CloseParen node -> node.Text
    | OpenBracket node -> node.Text
    | CloseBracket node -> node.Text
    | OpenSquareBracket node -> node.Text
    | CloseSquareBracket node -> node.Text
    | Power node -> node.Text
    | NotEquals node -> node.Text
    | LessThanOrEquals node -> node.Text
    | GreaterThanOrEquals node -> node.Text
    | And node -> node.Text
    | Or node -> node.Text
    | For node -> node.Text
    | While node -> node.Text
    | Until node -> node.Text
    | Do node -> node.Text
    | If node -> node.Text
    | Then node -> node.Text
    | Else node -> node.Text

  let span =
    function
    | EOF -> Span.FromBounds 0 0
    | Bad node -> node.Span
    | WhiteSpace node -> node.Span
    | Literal (node, _) -> node.Span
    | Identifier node -> node.Span
    | Plus node -> node.Span
    | Minus node -> node.Span
    | Star node -> node.Span
    | Slash node -> node.Span
    | BackSlash node -> node.Span
    | Percent node -> node.Span
    | Equals node -> node.Span
    | LessThan node -> node.Span
    | GreaterThan node -> node.Span
    | ExclamationMark node -> node.Span
    | QuestionMark node -> node.Span
    | Pipe node -> node.Span
    | Ampersant node -> node.Span
    | Dot node -> node.Span
    | Comma node -> node.Span
    | Quote node -> node.Span
    | DoubleQuote node -> node.Span
    | OpenParen node -> node.Span
    | CloseParen node -> node.Span
    | OpenBracket node -> node.Span
    | CloseBracket node -> node.Span
    | OpenSquareBracket node -> node.Span
    | CloseSquareBracket node -> node.Span
    | Power node -> node.Span
    | NotEquals node -> node.Span
    | LessThanOrEquals node -> node.Span
    | GreaterThanOrEquals node -> node.Span
    | And node -> node.Span
    | Or node -> node.Span
    | For node -> node.Span
    | While node -> node.Span
    | Until node -> node.Span
    | Do node -> node.Span
    | If node -> node.Span
    | Then node -> node.Span
    | Else node -> node.Span
