type definefun = string * (string list * PiSyntax.value)
type definefuns = definefun list


val apply_prog : definefuns -> SeqSyntax.prog -> SeqSyntax.prog
