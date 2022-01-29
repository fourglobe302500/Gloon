[<RequireQualifiedAccess>]
module Gloon.Lexer

open System

let private (|IsLetter|_|) c =
  if Char.IsLetter c then
    Some <| string c
  else
    None

let private (|IsNumber|_|) c =
  if Char.IsNumber c then
    Some(int c - int '0', string c)
  else
    None

let private (|IsWhiteSpace|_|) c =
  if Char.IsWhiteSpace c then
    Some <| string c
  else
    None

let private map i =
  function
  | IsLetter l -> Identifier(SyntaxNode.Basic l (i + 1))
  | IsNumber (n, t) -> Literal(SyntaxNode.Basic t (i + 1), Int n)
  | IsWhiteSpace w -> WhiteSpace <| SyntaxNode.Basic w (i + 1)
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

let private (|SN|) node = SN(node.Text, node.Span)

let private (|TextAndSpan|) token =
  TextAndSpan(SyntaxToken.text token, SyntaxToken.span token)

let private (|ValideInIdentifier|_|) =
  function
  | Bad (SN (t, s)) when t = "_" -> Some(t, s)
  | Quote (SN (t, s))
  | Literal (SN (t, s), Int _)
  | Identifier (SN (t, s)) -> Some(t, s)
  | _ -> None

let private createSN (text, span) = { Text = text; Span = span }

type private Flags =
  { Escaping: bool
    MakingString: bool
    MakingChar: bool }


let private compresser tokens =

  let rec f value tokens flags : (SyntaxToken * string list) list =
    match value, tokens, flags with
    //Fisrt Token
    | (t, d), [], _ -> [ t, "Missing End Of File Token" :: d ]
    | (t, d), EOF :: _, _ -> [ t, d ]

    //Float Parsing
    | (Literal (SN (t1, s1), Float _), d), Literal (SN (t2, s2), Int _) :: tail, _ // float + int -> float
    | (Literal (SN (t1, s1), Int _), d), Dot (SN (t2, s2)) :: tail, _ // int + dot   -> float
    | (Dot (SN (t1, s1)), d), Literal (SN (t2, s2), Int _) :: tail, _ -> // dot + int   -> float
      let fText = t1 + t2

      f (Literal(createSN (fText, s1 + s2), Float <| float fText), d) tail flags

    // Int Parsing
    | (Literal (SN (t1, s1), Int _), d), Literal (SN (t2, s2), Int _) :: tail, _ -> // int + int -> int
      let fText = t1 + t2

      f (Literal(createSN (fText, s1 + s2), Int <| int fText), d) tail flags

    // String Parsing
    | (Literal (SN (t1, s1), String s), d), BackSlash (SN (t2, s2)) :: tail, { MakingString = true } -> // "string + / -> "string/"
      f (Literal(createSN (t1 + t2, s1 + s2), Literal.String s), d) tail { flags with Escaping = true }
    | (Literal (SN (t1, s1), String s), d),
      DoubleQuote (SN (t2, s2)) :: tail,
      { Escaping = false
        MakingString = true } -> // "string + " -> "string"
      let fText = t1 + t2

      f (Literal(createSN (fText, s1 + s2), Literal.String s), d) tail { flags with MakingString = false }
    | (Literal (SN (t1, s1), String s), d), node :: tail, { Escaping = true; MakingString = true } -> // "string/ + char -> "string (error)
      let (t2, t2es, err) =
        let t = SyntaxToken.text node

        match t with
        | "n" -> t, "\n", []
        | "r" -> t, "\r", []
        | "t" -> t, "\t", []
        | "\"" -> t, "\"", []
        | "\\" -> t, "\\", []
        | "0" -> t, "", []
        | _ -> t, "", [ sprintf "Unknown Escaped Character '%s'" t ]

      let s2 = SyntaxToken.span node

      match err with
      | e :: _ -> f (Literal(createSN (t1 + t2, s1), Literal.String s), e :: d) tail { flags with Escaping = false }
      | [] -> f (Literal(createSN (t1 + t2, s1 + s2), Literal.String(s + t2es)), d) tail { flags with Escaping = false }


    | (Literal (SN (t1, s1), String s), d), node :: tail, { MakingString = true } -> // "string + char -> "string
      let t2 = SyntaxToken.text node
      let fText = t1 + t2

      f (Literal(createSN (fText, s1 + 1), Literal.String(s + t2)), d) tail flags

    | (DoubleQuote (SN (t1, s1)), d), DoubleQuote (SN (t2, s2)) :: tail, _ -> // " + " -> ""
      f (Literal(createSN (t1 + t2, s1 + s2), Literal.String ""), d) tail flags
    | (DoubleQuote (SN (t1, s1)), d), BackSlash (SN (t2, s2)) :: tail, _ -> // " + / -> "string/
      f
        (Literal(createSN (t1 + t2, s1 + s2), Literal.String ""), d)
        tail
        { flags with
            MakingString = true
            Escaping = true }
    | (DoubleQuote (SN (t1, s1)), d), TextAndSpan (t2, s2) :: tail, _ -> // " + any -> "string
      f (Literal(createSN (t1 + t2, s1 + s2), Literal.String t2), d) tail { flags with MakingString = true }

    // Char Parsing
    | (Literal (SN (t1, s1), Char s), d), Quote (SN (t2, s2)) :: tail, { Escaping = false; MakingChar = true } -> // 'char + ' -> 'char'
      f (Literal(createSN (t1 + t2, s1 + s2), Literal.Char s), d) tail { flags with MakingChar = false }
    | (Literal (SN (t1, s1), Char s), d), node :: tail, { Escaping = true; MakingChar = true } -> // 'char/ + char -> 'char (error)
      let (t2, t2es, err) =
        let t = SyntaxToken.text node

        match t with
        | "n" -> t, '\n', []
        | "r" -> t, '\r', []
        | "t" -> t, '\t', []
        | "\"" -> t, '\"', []
        | "\\" -> t, '\\', []
        | "0" -> t, '\000', []
        | _ -> t, '\000', [ sprintf "Unknown Escaped Character '%s'" t ]

      match err with
      | e :: _ -> f (Literal(createSN (t1 + t2, s1), Literal.Char s), e :: d) tail { flags with Escaping = false }
      | [] ->
        f
          (Literal(createSN (t1 + t2, s1 + SyntaxToken.span node), Literal.Char(s + t2es)), d)
          tail
          { flags with Escaping = false }
    | (Quote (SN (t1, s1)), d), Quote (SN (t2, s2)) :: tail, _ -> // ' + ' -> ''
      f (Literal(createSN (t1 + t2, s1 + s2), Literal.Char '\000'), "Invallid Char  \"''\"" :: d) tail flags
    | (Quote (SN (t1, s1)), d), BackSlash (SN (t2, s2)) :: tail, _ -> // ' + / -> 'char/
      f
        (Literal(createSN (t1 + t2, s1 + s2), Literal.Char '\000'), d)
        tail
        { flags with
            MakingChar = true
            Escaping = true }
    | (Quote (SN (t1, s1)), d), TextAndSpan (t2, s2) :: tail, _ -> // ' + any -> 'char
      let (char, diags) =
        match t2 |> List.ofSeq with
        | c :: [] -> c, []
        | _ -> '\000', [ "Invallid char" ]

      match diags with
      | e :: [] ->
        f (Literal(createSN (t1 + t2, s1 + s2), Literal.Char char), e :: d) tail { flags with MakingChar = true }
      | _ -> f (Literal(createSN (t1 + t2, s1 + s2), Literal.Char char), d) tail { flags with MakingChar = true }

    // Operators compression
    | (Star (SN (t1, s1)), d), Star (SN (t2, s2)) :: tail, _ -> // **
      f (Power(createSN (t1 + t2, s1 + s2)), d) tail flags
    | (LessThan (SN (t1, s1)), d), Equals (SN (t2, s2)) :: tail, _ -> // <=
      f (LessThanOrEquals(createSN (t1 + t2, s1 + s2)), d) tail flags
    | (GreaterThan (SN (t1, s1)), d), Equals (SN (t2, s2)) :: tail, _ -> // >=
      f (GreaterThanOrEquals(createSN (t1 + t2, s1 + s2)), d) tail flags
    | (ExclamationMark (SN (t1, s1)), d), Equals (SN (t2, s2)) :: tail, _ -> // !=
      f (NotEquals(createSN (t1 + t2, s1 + s2)), d) tail flags
    | (Pipe (SN (t1, s1)), d), Pipe (SN (t2, s2)) :: tail, _ -> // ||
      f (Or(createSN (t1 + t2, s1 + s2)), d) tail flags
    | (Ampersant (SN (t1, s1)), d), Ampersant (SN (t2, s2)) :: tail, _ -> // &&
      f (And(createSN (t1 + t2, s1 + s2)), d) tail flags

    // Whitespace
    | l, WhiteSpace w :: tail, _ -> l :: f (WhiteSpace w, []) tail flags

    // Operators
    | l, Plus v :: tail, _ -> l :: f (Plus v, []) tail flags
    | l, Minus v :: tail, _ -> l :: f (Minus v, []) tail flags
    | l, Star v :: tail, _ -> l :: f (Star v, []) tail flags
    | l, Slash v :: tail, _ -> l :: f (Slash v, []) tail flags
    | l, Percent v :: tail, _ -> l :: f (Percent v, []) tail flags
    | l, OpenParen v :: tail, _ -> l :: f (OpenParen v, []) tail flags
    | l, OpenBracket v :: tail, _ -> l :: f (OpenBracket v, []) tail flags
    | l, OpenSquareBracket v :: tail, _ -> l :: f (OpenSquareBracket v, []) tail flags
    | l, CloseParen v :: tail, _ -> l :: f (CloseParen v, []) tail flags
    | l, CloseBracket v :: tail, _ -> l :: f (CloseBracket v, []) tail flags
    | l, CloseSquareBracket v :: tail, _ -> l :: f (CloseSquareBracket v, []) tail flags
    | l, Dot v :: tail, _ -> l :: f (Dot v, []) tail flags
    | l, Comma v :: tail, _ -> l :: f (Comma v, []) tail flags
    | l, Equals v :: tail, _ -> l :: f (Equals v, []) tail flags
    | l, LessThan v :: tail, _ -> l :: f (LessThan v, []) tail flags
    | l, GreaterThan v :: tail, _ -> l :: f (GreaterThan v, []) tail flags
    | l, ExclamationMark v :: tail, _ -> l :: f (ExclamationMark v, []) tail flags
    | l, QuestionMark v :: tail, _ -> l :: f (QuestionMark v, []) tail flags
    | l, Pipe v :: tail, _ -> l :: f (Pipe v, []) tail flags
    | l, Ampersant v :: tail, _ -> l :: f (Ampersant v, []) tail flags

    // Identifier Compression
    | (Identifier (SN (t1, s1)), d), ValideInIdentifier (t2, s2) :: tail, _ -> // identifier + any identifier -> identifier
      f (Identifier(createSN (t1 + t2, s1 + s2)), d) tail flags

    // PassThru
    | l, t :: tail, _ -> l :: f (t, []) tail flags

  match tokens with
  | [] -> [ EOF ], []
  | t :: tail ->
    f
      (t, [])
      (tail @ [ EOF ])
      { Escaping = false
        MakingString = false
        MakingChar = false }
    |> fun l -> List.foldBack (fun (t, d) (tokens, diags) -> t :: tokens, d @ diags) l ([], [])


let lex (s: string) =
  List.ofSeq s
  |> List.mapi map
  |> compresser
  // Using the id there for the eventual Diagnostics type
  |> fun (tokens, diags) -> tokens, diags |> List.map id
