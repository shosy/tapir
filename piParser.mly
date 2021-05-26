%{
open PiSyntax
let inittype = ()
let addtype x = (x, inittype)
%}

%token <string> IDENT
%token <bool> BOOL
%token <int> INT
%token ZERO
%token NU
%token IN
%token INPUT
%token OUTPUT
%token PERIOD
%token AST
%token PAR
%token IF
%token THEN
%token ELSE
%token LPAREN
%token RPAREN
%token COMMA
%token NOT
%token AND
%token OR
%token EQ
%token LT
%token GT
%token LE
%token GE
%token PLUS
%token MINUS
%token SLASH
%token EOF

%nonassoc IN
%left PAR
%nonassoc ELSE
%nonassoc PERIOD
%left OR
%left AND
%left EQ LT GT LE GE
%left PLUS MINUS
%left AST SLASH
%nonassoc SIGN
%left NOT

%start toplevel
%type <unit PiSyntax.proc> toplevel

%%

toplevel:
  proc EOF
    { $1 }
;

proc:
  ZERO
    { Nil }
| NU IDENT IN proc
    { Nu($2, inittype, $4) }
| IDENT INPUT ident_pattern PERIOD proc
    { In($1, List.map addtype $3, $5) }
| IDENT INPUT ident_pattern
    { In($1, List.map addtype $3, Nil) }
| AST IDENT INPUT ident_pattern PERIOD proc
    { RIn($2, List.map addtype $4, $6) }
| AST IDENT INPUT ident_pattern
    { RIn($2, List.map addtype $4, Nil) }
| IDENT OUTPUT value_pattern PERIOD proc
    { Out($1, $3, $5) }
| IDENT OUTPUT value_pattern 
    { Out($1, $3, Nil) }
| proc PAR proc
    { Par($1, $3) } 
| IF value THEN proc ELSE proc
    { If($2, $4, $6) }
| LPAREN proc RPAREN
    { $2 }
;

ident_pattern:
  LPAREN RPAREN    /* no arguments */
    { [] }
| IDENT    /* one argument */
    { [$1] }
| LPAREN IDENT COMMA rev_ident_list RPAREN    /* at least two arguments */
    { $2 :: List.rev $4 }   /* あとで戻す */

rev_ident_list:
  IDENT
    { [$1] }
| rev_ident_list COMMA IDENT
    { if List.mem $3 $1 then failwith ("Variable "^$3^" is bound several times in this matching");
      $3 :: $1 }
;

value_pattern:
  LPAREN RPAREN    /* no arguments */
    { [] }
| value    /* one argument */
    { [$1] }
| LPAREN value COMMA value_list RPAREN    /* at least two arguments */
    { $2 :: $4 }  /* あとで戻す */

value_list:
  value
    { [$1] }
| value COMMA value_list
    { $1 :: $3 }

value:
  IDENT
    { Var($1) }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| NOT value
    { Op(NOT, [$2]) } 
| value AND value
    { Op(AND, [$1; $3]) }
| value OR value
    { Op(OR, [$1; $3]) }
| value EQ value
    { Op(EQ, [$1; $3]) }
| value LT value
    { Op(LT, [$1; $3]) }
| value GT value
    { Op(GT, [$1; $3]) }
| value LE value
    { Op(LE, [$1; $3]) }
| value GE value
    { Op(GE, [$1; $3]) }
| MINUS value  %prec SIGN
    { Op(MINUS, [$2]) }
| value PLUS value
    { Op(ADD, [$1; $3]) }
| value MINUS value
    { Op(SUB, [$1; $3]) }
| value AST value 
    { Op(MUL, [$1; $3]) }
| value SLASH value
    { Op(DIV, [$1; $3]) }
| LPAREN value RPAREN
    { $2 }
;
