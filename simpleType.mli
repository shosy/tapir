(** syntax of simple types **)
type t =
    | SBool
    | SInt
    | SCh of t list * int    (* the int parameter denotes the region of the channel *)

(** external type environment **)
val extenv : (string, t) M.t ref 

(** types of operators **)
(* val op_table : (PiSyntax.op * (t list * t)) list *)

(** for pretty-print **)
val pp_print_t : Format.formatter -> t -> unit
