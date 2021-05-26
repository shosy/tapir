type t =
    (* | RBool  *)
    | RInt of string * PiSyntax.value
    | RCh of (string * t) list * int


type rTypeEnvEl = Bind of string * t | Val of PiSyntax.value 
type rTypeEnv = rTypeEnvEl list
    
val extenv : rTypeEnv ref


(** pretty-print **)
val pp_print_t : Format.formatter -> t -> unit

val subst_t : (string, PiSyntax.value) M.t -> t -> t
