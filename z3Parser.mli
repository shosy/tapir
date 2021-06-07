type token =
  | IDENT of (string)
  | BOOL of (bool)
  | INT of (int)
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | GT
  | LE
  | GE
  | PLUS
  | MINUS
  | AST
  | SLASH
  | LPAREN
  | RPAREN
  | EXISTS
  | LET
  | EXCLAMATION
  | WEIGHT
  | EOF

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ModelSyntax.definefuns
