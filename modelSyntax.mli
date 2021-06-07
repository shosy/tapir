type definefun = string * (string list * PiSyntax.value)
type definefuns = definefun list


val del_exists : definefuns -> definefuns 
val del_unknown : definefuns -> definefuns 
val merge : definefuns -> definefuns -> definefuns

val apply_prog : definefuns -> SeqSyntax.prog -> SeqSyntax.prog
