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

open Parsing;;
let _ = parse_error;;
# 2 "z3Parser.mly"
open ModelSyntax
# 30 "z3Parser.ml"
let yytransl_const = [|
  260 (* NOT *);
  261 (* AND *);
  262 (* OR *);
  263 (* EQ *);
  264 (* LT *);
  265 (* GT *);
  266 (* LE *);
  267 (* GE *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* AST *);
  271 (* SLASH *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* EXISTS *);
  275 (* LET *);
  276 (* EXCLAMATION *);
  277 (* WEIGHT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* BOOL *);
  259 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\006\000\006\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\000\000"

let yylen = "\002\000\
\006\000\005\000\001\000\002\000\009\000\008\000\004\000\005\000\
\001\000\002\000\001\000\001\000\001\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\007\000\004\000\003\000\007\000\006\000\004\000\005\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\004\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\012\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\000\000\008\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\010\000\027\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\026\000\
\000\000\029\000\032\000"

let yydgoto = "\002\000\
\004\000\009\000\010\000\020\000\052\000\053\000\086\000"

let yysindex = "\005\000\
\006\255\000\000\250\254\000\000\010\255\242\254\023\255\029\000\
\013\255\015\255\031\255\000\000\033\000\000\000\018\255\000\000\
\244\254\034\255\051\255\036\255\053\255\056\255\054\255\043\255\
\000\000\000\000\000\000\008\255\044\255\056\255\046\255\048\255\
\056\255\056\255\056\255\056\255\056\255\056\255\056\255\056\255\
\056\255\056\255\056\255\056\255\047\255\055\255\056\255\000\000\
\052\255\000\000\000\000\056\255\057\255\058\255\059\255\060\255\
\061\255\062\255\063\255\064\255\065\255\066\255\067\255\068\255\
\069\255\046\255\071\255\049\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\072\255\087\255\073\255\070\255\056\255\
\056\255\056\255\074\255\075\255\076\255\077\255\000\000\000\000\
\071\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\078\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\079\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\080\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\088\000\000\000\226\255\234\255\004\000\002\000"

let yytablesize = 99
let yytable = "\029\000\
\050\000\007\000\008\000\018\000\019\000\001\000\003\000\049\000\
\032\000\005\000\006\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\011\000\
\068\000\045\000\046\000\047\000\012\000\013\000\007\000\015\000\
\016\000\017\000\021\000\084\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\025\000\026\000\027\000\022\000\023\000\024\000\030\000\070\000\
\025\000\026\000\027\000\031\000\048\000\018\000\066\000\028\000\
\051\000\092\000\093\000\094\000\069\000\087\000\067\000\028\000\
\091\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\085\000\089\000\
\088\000\090\000\095\000\096\000\097\000\098\000\003\000\007\000\
\009\000\014\000\099\000"

let yycheck = "\022\000\
\031\000\016\001\017\001\016\001\017\001\001\000\001\001\030\000\
\001\001\016\001\001\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\001\001\
\047\000\018\001\019\001\020\001\000\000\017\001\016\001\001\001\
\000\000\016\001\001\001\066\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\001\001\002\001\003\001\001\001\017\001\001\001\001\001\052\000\
\001\001\002\001\003\001\017\001\017\001\016\001\016\001\016\001\
\017\001\088\000\089\000\090\000\017\001\021\001\016\001\016\001\
\003\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
\017\001\017\001\017\001\017\001\017\001\017\001\016\001\001\001\
\017\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
\017\001\010\000\097\000"

let yynames_const = "\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  PLUS\000\
  MINUS\000\
  AST\000\
  SLASH\000\
  LPAREN\000\
  RPAREN\000\
  EXISTS\000\
  LET\000\
  EXCLAMATION\000\
  WEIGHT\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  BOOL\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'definefun_list) in
    Obj.repr(
# 36 "z3Parser.mly"
    ( _4 )
# 192 "z3Parser.ml"
               : ModelSyntax.definefuns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 38 "z3Parser.mly"
    ( [] )
# 200 "z3Parser.ml"
               : ModelSyntax.definefuns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'definefun) in
    Obj.repr(
# 43 "z3Parser.mly"
    ( [_1] )
# 207 "z3Parser.ml"
               : 'definefun_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'definefun) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'definefun_list) in
    Obj.repr(
# 45 "z3Parser.mly"
    ( _1 :: _2 )
# 215 "z3Parser.ml"
               : 'definefun_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'ident_type_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 49 "z3Parser.mly"
    ( (_3, (_5, _8)) )
# 226 "z3Parser.ml"
               : 'definefun))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 51 "z3Parser.mly"
    ( (_3, ([], _7)) )
# 236 "z3Parser.ml"
               : 'definefun))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "z3Parser.mly"
    ( [_2] )
# 244 "z3Parser.ml"
               : 'ident_type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'ident_type_list) in
    Obj.repr(
# 58 "z3Parser.mly"
    ( _2 :: _5 )
# 253 "z3Parser.ml"
               : 'ident_type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 63 "z3Parser.mly"
    ( [_1] )
# 260 "z3Parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value_list) in
    Obj.repr(
# 65 "z3Parser.mly"
    ( _1 :: _2 )
# 268 "z3Parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "z3Parser.mly"
    ( Var(_1) )
# 275 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 71 "z3Parser.mly"
    ( Bool(_1) )
# 282 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "z3Parser.mly"
    ( Int(_1) )
# 289 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 75 "z3Parser.mly"
    ( Op(NOT, _3) )
# 296 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 77 "z3Parser.mly"
    ( Op(AND, _3) )
# 303 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 79 "z3Parser.mly"
    ( Op(OR, _3) )
# 310 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 81 "z3Parser.mly"
    ( Op(EQ, _3) )
# 317 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 83 "z3Parser.mly"
    ( Op(LT, _3))
# 324 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 85 "z3Parser.mly"
    ( Op(GT, _3))
# 331 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 87 "z3Parser.mly"
    ( Op(LE, _3))
# 338 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 89 "z3Parser.mly"
    ( Op(GE, _3))
# 345 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 91 "z3Parser.mly"
    ( Op(ADD, _3) )
# 352 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 93 "z3Parser.mly"
    ( if List.length _3 = 1 then Op(MINUS, _3) else Op(SUB, _3) )
# 359 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 95 "z3Parser.mly"
    ( Op(MUL, _3) )
# 366 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 97 "z3Parser.mly"
    ( Op(DIV, _3) )
# 373 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident_type_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 99 "z3Parser.mly"
    ( Exists(_4, _6) )
# 381 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 101 "z3Parser.mly"
    ( Unknown(_2, _3) )
# 389 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "z3Parser.mly"
    ( Unknown(_2, []) )
# 396 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'let_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 105 "z3Parser.mly"
    ( PiSyntax.subst_val _4 _6 )
# 404 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'value) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 107 "z3Parser.mly"
    ( _3 )
# 412 "z3Parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 112 "z3Parser.mly"
    ( M.singleton _2 _3 )
# 420 "z3Parser.ml"
               : 'let_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'let_list) in
    Obj.repr(
# 114 "z3Parser.mly"
    ( M.add _2 _3 _5 )
# 429 "z3Parser.ml"
               : 'let_list))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ModelSyntax.definefuns)
