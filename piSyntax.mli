(** operators **)
type op = NOT | AND | OR | IMPLY | EQ | LT | GT | LE | GE | MINUS | ADD | SUB | MUL | DIV

(** syntax of values **)
type value =
    | Var of string
    | Bool of bool
    | Int of int
    | Op of op * value list
    | Unknown of string * value list
    | Exists of string list * value

(** syntax of processes **)
(** the parameter 'a denotes the type of type expressions for bound variables **)
type 'a proc =
    | Nil
    | Nu of string * 'a * 'a proc
    | In of string * (string * 'a) list * 'a proc
    | RIn of string * (string * 'a) list * 'a proc
    | Out of string * value list * 'a proc
    | Par of 'a proc * 'a proc
    | If of value * 'a proc * 'a proc

(** pretty-print **)
val pp_print_val : Format.formatter -> value -> unit
val pp_print_proc : ?pp_print_t:(Format.formatter -> 'a -> unit) -> Format.formatter -> 'a proc -> unit

val print_val : out_channel -> value -> unit
val print_proc : ?pp_print_t:(Format.formatter -> 'a -> unit) -> out_channel -> 'a proc -> unit

val subst_val : (string, value) M.t -> value -> value
val subst_proc : (string, value) M.t -> 'a proc -> 'a proc
