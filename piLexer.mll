{
open PiParser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+    { token lexbuf }
| "/*"      { comment lexbuf; token lexbuf }
| "O"       { ZERO }    (* not 0 but the 15th letter of the English alphabet *)
| "new"     { NU }
| "in"      { IN }
| "?"       { INPUT }
| "!"       { OUTPUT }
| "."       { PERIOD }
| "*"       { AST }
| "|"       { PAR }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "("       { LPAREN }
| ")"       { RPAREN }
| ","       { COMMA }
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
| "/"       { SLASH }
| eof       { EOF }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| lower (digit|lower|upper)*
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { Format.eprintf "unknown token %s near characters %d-%d@."
	    (Lexing.lexeme lexbuf)
	    (Lexing.lexeme_start lexbuf)
	    (Lexing.lexeme_end lexbuf);
      failwith "lex error" }
and comment = parse
| "*/"
    { () }
| "/*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { failwith "unterminated comment" }
| _
    { comment lexbuf }
