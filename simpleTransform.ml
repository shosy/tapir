open PiSyntax
open SimpleType
open SeqSyntax

let function_name = function
    | SCh(_,i) -> "function" ^ string_of_int i
    | _ -> assert false


let (+) (f1 : (SimpleType.t, ((string, SimpleType.t) M.t * string list * SeqSyntax.expr) list) M.t) f2 =
    M.merge
        (fun t tmp1 tmp2 -> 
            match tmp1, tmp2 with
            | Some(l1), Some(l2) -> Some(l1 @ l2)
            | Some(l1), None -> Some(l1)
            | None, Some(l2) -> Some(l2)
            | _ -> None)
        f1
        f2

let empty_f (env : (string, SimpleType.t) M.t) =
    (* ch-type \mapsto \epsilon  *)
    M.fold_left 
        (fun f _ t -> match t with SCh(_) -> f + M.singleton t [] | _ -> f)
        M.empty
        env

let substnondet yts e = 
    List.fold_right 
        (fun (y,t) e -> match t with SBool | SInt -> LetNonDet(y,e) | SCh(_) -> e) 
        yts
        e

let delch bindings = List.map fst (List.filter (fun (_,t) -> t = SBool || t = SInt) bindings)

let rec trans env = function
    | Nil -> Skip, empty_f env
    | Nu(x,t,p) -> trans (M.add x t env) p
    | In(_,yts,p) ->
        let e,f = trans (M.add_list yts env) p in 
        substnondet yts e, f
    | RIn(x,yts,p) -> 
        let t = M.find x env in
        let e,f = trans (M.add_list yts env) p in
        Skip, (M.singleton t [(env, delch yts, e)]) + f
    | Out(x,vs,p) -> 
        let t = M.find x env in  (* is_type x env *)
        let vts = List.map (fun v -> match v with Var(x) -> (v, M.find x env) | _ -> (v, SInt)) vs in  (*bool*) (* type_of v env *)
        let e,f = trans env p in
        Choice(Call(function_name t, delch vts), e), f
    | Par(p1,p2) -> 
        let e1,f1 = trans env p1 in
        let e2,f2 = trans env p2 in
        Choice(e1, e2), f1 + f2
    | If(v,p1,p2) -> 
        let e1,f1 = trans env p1 in
        let e2,f2 = trans env p2 in
        If(v, e1, e2), f1 + f2


let close env e = 
    M.fold_right 
        (fun x t e -> match t with SBool | SInt -> LetNonDet(x,e) | SCh(_) -> e)
        env 
        e
 
let num = ref 0
let makefunc (f : (SimpleType.t, ((string, SimpleType.t) M.t * string list * SeqSyntax.expr) list) M.t) =
    M.fold_left
        (fun fundefs t l -> 
            let new_args = match t with SCh(ts,_) -> List.map (fun _ -> incr num; "arg" ^ string_of_int !num) (List.filter ((=) SInt  (*SBool*)) ts) | _ -> assert false in
            fundefs @ [(function_name t, new_args, List.fold_left (fun expr (env,ys,e) -> Choice(expr, close env (subst_expr (M.of_list2 ys (List.map (fun z -> Var(z)) new_args)) e))) Skip l)])
        []
        f

let makeprog (e,f) = (makefunc f, e)


let transform p = 
    let e,f = trans !extenv p in
    makeprog (e,f)
