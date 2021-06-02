(** syntax of simple types **)
type t =
    | SBool
    | SInt
    | SCh of t list * int    (* the int parameter denotes the region of the channel *)

(** external type environment **)
val extenv : (string, t) M.t ref 
val ext_bienv : (string, t) M.t ref 

(** types of operators **)
(* val op_table : (PiSyntax.op * (t list * t)) list *)

(** for pretty-print **)
val pp_print_t : Format.formatter -> t -> unit


val is_bool : t -> bool
val is_int : t -> bool
val is_bool_or_int : t -> bool
val is_ch : t -> bool

val type_of : (string, t) M.t -> (string, t) M.t -> PiSyntax.value -> t
