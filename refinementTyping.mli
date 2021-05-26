val typing : SimpleType.t PiSyntax.proc -> RefinementType.t PiSyntax.proc * PiSyntax.value list

val pp_print_smt2 : Format.formatter -> PiSyntax.value list -> unit                                      

val print_smt2 : out_channel -> PiSyntax.value list -> unit
