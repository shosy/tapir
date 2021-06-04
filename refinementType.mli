(* type t =
    (* | RBool  *)
    | RInt of string * PiSyntax.value
    | RCh of (string * t) list * int *)
    type t =
        | RBool
        | RInt
        | RCh of (string * t) list * PiSyntax.value * t list * PiSyntax.value * t list * int



(* type rTypeEnvEl = Bind of string * t | Val of PiSyntax.value  *)
(* type rTypeEnv = rTypeEnvEl list *)
    
val ext_bienv : (string, t) M.t ref


(** pretty-print **)
val pp_print_t : Format.formatter -> t -> unit

val subst_t : (string, PiSyntax.value) M.t -> t -> t


val type_of : (string, t) M.t -> (string, t) M.t -> PiSyntax.value -> t

val is_bool : t -> bool
val is_int : t -> bool
val is_bool_or_int : t -> bool
val is_ch : t -> bool
