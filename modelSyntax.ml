open PiSyntax
open SeqSyntax

type definefun = string * (string list * value)
type definefuns = definefun list




let rec fv_val = function
    | Var(x) -> S.singleton x
    | Bool(_) | Int(_) -> S.empty
    | Op(_,vs) | Unknown(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs
    | Exists(xs,v) -> S.diff (fv_val v) (S.of_list xs)

let rec del_exists is_positive = function
    | Exists(xs,Op(AND,vs)) when is_positive -> 
        Op(AND, List.map (fun v -> if List.exists (fun x -> S.mem x (fv_val v)) xs then Bool(true) else del_exists is_positive v) vs)
    | Exists(_,v) when is_positive -> Bool(true)
    | Exists(_,v) when not is_positive -> Bool(false)
    | Op(NOT,vs) -> Op(NOT, List.map (del_exists (not is_positive)) vs) 
    | Op(IMPLY,[v1;v2]) -> Op(IMPLY, [del_exists (not is_positive) v1; del_exists is_positive v2])
    | Op(op,vs) -> Op(op, List.map (del_exists is_positive) vs)
    | v -> v
        
let del_exists definefuns = 
    List.map (fun (phi,(xs,v)) -> (phi, (xs, del_exists true v))) definefuns




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
