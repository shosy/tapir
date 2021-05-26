%{
open ModelSyntax
%}

%token <string> IDENT
%token <bool> BOOL
%token <int> INT
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
%token AST
%token SLASH
%token LPAREN
%token RPAREN
%token EXISTS
%token EOF

%start toplevel
%type <ModelSyntax.definefuns> toplevel

%%

toplevel:
  IDENT LPAREN IDENT definefun_list RPAREN EOF
  /* sat (model .... ) */
    { $4 }
| IDENT LPAREN IDENT RPAREN EOF    /* for nil process */
    { [] }
;

definefun_list:
  definefun
    { [$1] }
| definefun definefun_list
    { $1 :: $2 }

definefun:
  LPAREN IDENT IDENT LPAREN ident_list RPAREN IDENT value RPAREN  /* arglist = [] */
    { ($3, ($5, $8)) }
;

ident_list:
  LPAREN IDENT IDENT RPAREN
    { [$2] }
| LPAREN IDENT IDENT RPAREN ident_list
    { $2 :: $5 }
;

value_list:
  value
    { [$1] }
| value value_list
    { $1 :: $2 }

value:
  IDENT
    { Var($1) }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| LPAREN NOT value_list RPAREN
    { Op(NOT, $3) }
| LPAREN AND value_list RPAREN
    { Op(AND, $3) }
| LPAREN OR value_list RPAREN
    { Op(OR, $3) }
| LPAREN EQ value_list RPAREN
    { Op(EQ, $3) }
| LPAREN LT value_list RPAREN
    { Op(LT, $3)}
| LPAREN GT value_list RPAREN
    { Op(GT, $3)}
| LPAREN LE value_list RPAREN
    { Op(LE, $3)}
| LPAREN GE value_list RPAREN
    { Op(GE, $3)}
| LPAREN PLUS value_list RPAREN
    { Op(ADD, $3) }
| LPAREN MINUS value_list RPAREN
    { if List.length $3 = 1 then Op(MINUS, $3) else Op(SUB, $3) }
| LPAREN AST value_list RPAREN
    { Op(MUL, $3) }
| LPAREN SLASH value_list RPAREN
    { Op(DIV, $3) }
| LPAREN EXISTS LPAREN ident_list RPAREN value RPAREN
    { Bool(true) }    /* 相当まずい */
;
