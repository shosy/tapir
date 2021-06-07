open PiSyntax
open SeqSyntax

let rec extfun e0 =
    match e0 with
    | Skip -> []
    | Let(_,_,e) | LetNonDet(_,e) -> extfun e
    | Call(f1,vs) -> [Call(f1,vs)]
    | Choice(e1,e2) | If(_,e1,e2) -> extfun e1 @ extfun e2
    | Assume(phi,e) -> List.map (fun e -> Assume(phi,e)) (extfun e)

let flow = ref []


let rec fname = function
    | Call(f1,_) -> f1
    | Assume(_,e) -> fname e
    | _ -> assert false    


let rec trans xs = function
    | Call(_,vs) -> List.map2 (fun x v -> Op(EQ, [Var(x); v])) xs vs 
    | Assume(phi,e) -> phi :: trans xs e
    | _ -> assert false
    
let rec expand fundefs = function
    | Call(f1,vs) -> 
        let (_,xs,e) = List.find (fun (f,_,_) -> f = f1) fundefs in
        subst_expr (M.of_list2 xs vs) e
    | Assume(phi,e) -> Assume(phi, expand fundefs e)
    | _ -> assert false

let once = ref true

let rec main (fundefs,eee) =
    if !once then
        (flow := List.fold_left (fun ls (f,xs,e) -> ls @ List.map (fun e1 -> (f, xs, e1)) (extfun e)) [] fundefs;
         once := false);
    match !flow with
    | [] -> raise (Invalid_argument ("addCHC"))
    | (f,xs,e)::tl -> 
        if f = fname e then 
            (flow := tl;
             Op(IMPLY, [Op(AND, trans xs e); Bool(false)]))
        else 
            (flow := tl @ List.map (fun e1 -> (f, xs, e1)) (extfun (expand fundefs e));
             main (fundefs,eee))


