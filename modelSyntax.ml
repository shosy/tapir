open PiSyntax
open SeqSyntax

type definefun = string * (string list * value)
type definefuns = definefun list


let rec apply_val definefuns = function
    | Unknown(phi,vs) ->
        (try 
            let (xs,v) = List.assoc phi definefuns in 
            apply_val definefuns (subst_val (M.of_list2 xs vs) v)
        with 
            Not_found -> Unknown(phi,vs))
    | Op(op,vs) -> Op(op, List.map (apply_val definefuns) vs)
    | v -> v
let rec apply_expr definefuns = function
    | Skip -> Skip
    | Let(x,v,e) -> Let(x, v, apply_expr definefuns e)
    | LetNonDet(x,e) -> LetNonDet(x, apply_expr definefuns e)
    | Call(x,vs) -> Call(x, vs)
    | Choice(e1,e2) -> Choice(apply_expr definefuns e1, apply_expr definefuns e2)
    | If(v,e1,e2) -> If(v, apply_expr definefuns e1, apply_expr definefuns e2)
    | Assume(v,e) -> Assume(apply_val definefuns v, apply_expr definefuns e)
let apply_fundef definefuns (f,xs,e) = (f, xs, apply_expr definefuns e)
let apply_prog definefuns (fundefs,e) = 
    (List.map (apply_fundef definefuns) fundefs, apply_expr definefuns e)
