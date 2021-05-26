{
open ModelParser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+    { token lexbuf }
| "("       { LPAREN }
| ")"       { RPAREN }
| "true"    { BOOL(true) }
| "false"   { BOOL(false) }
| "not"     { NOT }
| "and"     { AND }
| "or"      { OR }
| "="       { EQ }
| "<"       { LT }
| ">"       { GT }
| "<="      { LE }
| ">="      { GE }
| "+"       { PLUS }
| "-"       { MINUS }
| "*"       { AST }
| "/"       { SLASH }
| eof       { EOF }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| (lower|upper|'_') (digit|lower|upper|'_')+
    { let s = Lexing.lexeme lexbuf in
      IDENT(s) }
| _
    { Format.eprintf "unknown token %s near characters %d-%d@."
	    (Lexing.lexeme lexbuf)
	    (Lexing.lexeme_start lexbuf)
	    (Lexing.lexeme_end lexbuf);
      failwith "lex error" }
