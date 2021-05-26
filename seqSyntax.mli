open PiSyntax

(** syntax of expressions **)
type expr = 
    | Skip 
    | Let of string * value * expr
    | LetNonDet of string * expr
    | Call of string * value list
    | Choice of expr * expr
    | If of value * expr * expr
    | Assume of value * expr

(** syntax of function definitions **)
type fundef = string * string list * expr

(** syntax of sequential programs **)
type prog = fundef list * expr


val pp_print_prog :  Format.formatter -> prog -> unit

val print_prog : out_channel -> prog -> unit


val subst_expr : (string, string) M.t -> expr -> expr
                          (* val *)