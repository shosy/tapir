open PiSyntax
open SimpleType
open RefinementType
open SeqSyntax




(* refinementTypingにも記載 *)
let rec add el = function
    | [] -> [el]
    | hd::tl -> 
        (match el, hd with
        | Bind(x,_), Bind(y,_) when x = y -> add el tl
        | _ -> hd :: add el tl)
let rec find x = function
    | [] -> raise Not_found
    | Bind(y,t)::_ when y = x -> t
    | _::env -> find x env
let add_list xts env = List.fold_left (fun env (x,t) -> add (Bind(x,t)) env) env xts 


let num0 = ref 0
let new_var () =
    incr num0;
    "_v" ^ string_of_int !num0




let function_name = function
    | SCh(_,i) -> "function" ^ string_of_int i
    | _ -> assert false


let rec simple = function
    | RInt(_) -> SInt
    | RCh(zts,i) -> SCh(List.map (fun (_,t) -> simple t) zts, i)


let (+) (f1 : (SimpleType.t, (rTypeEnv * string list * SeqSyntax.expr) list) M.t) f2 =
    M.merge
        (fun t tmp1 tmp2 -> 
            match tmp1, tmp2 with
            | Some(l1), Some(l2) -> Some(l1 @ l2)
            | Some(l1), None -> Some(l1)
            | None, Some(l2) -> Some(l2)
            | _ -> None)
        f1
        f2

let empty_f (env : rTypeEnv) : (SimpleType.t, (rTypeEnv * string list * SeqSyntax.expr) list) M.t =
    (* ch-type \mapsto \epsilon  *)
    List.fold_left 
        (fun f el -> match el with Bind(_,(RCh(_) as t)) -> f + M.singleton (simple t) [] | _ -> f)
        M.empty
        env

let substnondet yts e = 
    List.fold_right
        (fun (y,t) e -> match t with RInt(x,v) -> LetNonDet(y, Assume(subst_val (M.singleton x (Var(y))) v, e)) | RCh(_) -> e)
        yts
        e

let delch bindings = List.map fst (List.filter (fun (_,t) -> match t with RInt(_) -> true | RCh(_) -> false) bindings)  (* RBool *)

let of_type v env = 
    match v with
    | Var(x) -> 
        (match find x env with   RCh(_) as t -> t 
                               | RInt(_) -> let x = new_var () in RInt(x, Op(EQ, [Var(x); v])))
    | _ -> let x = new_var () in RInt(x, Op(EQ, [Var(x); v]))
    
let rec trans env = function
    | Nil -> Skip, empty_f env
    | Nu(x,t,p) -> trans (add (Bind(x, t)) env) p
    | In(_,yts,p) -> 
        let e,f = trans (add_list yts env) p in
        substnondet yts e, f
    | RIn(x,yts,p) ->
        let t = find x env in
        let e,f = trans (add_list yts env) p in
        Skip, (M.singleton (simple t) [(env, delch yts, e)]) + f
    | Out(x,vs,p) ->
        let t = find x env in
        let vts = List.map (fun v -> (v, of_type v env)) vs in  (* value -> そのtypeを作る事 *)
        let e,f = trans env p in
        Choice(Call(function_name (simple t), delch vts), e), f
    | Par(p1,p2) -> 
        let e1,f1 = trans env p1 in
        let e2,f2 = trans env p2 in
        Choice(e1, e2), f1 + f2
    | If(v,p1,p2) -> 
        let e1,f1 = trans (add (Val(v)) env) p1 in
        let e2,f2 = trans (add (Val(Op(NOT,[v]))) env) p2 in
        If(v, e1, e2), f1 + f2
        

let close env e = 
    List.fold_right 
        (fun el e -> match el with   Bind(x,RInt(y,v)) -> LetNonDet(x, Assume(subst_val (M.singleton y (Var(x))) v, e)) 
                                   | Bind(x,RCh(_)) -> e
                                   | Val(v) -> Assume(v, e))
        env
        e 

let num = ref 0
let makefunc (f : (SimpleType.t, (rTypeEnv * string list * SeqSyntax.expr) list) M.t) =
    M.fold_left
        (fun fundefs t l -> 
            let new_args = match t with SCh(ts,_) -> List.map (fun _ -> incr num; "arg" ^ string_of_int !num) (List.filter ((=) SInt) ts) | _ -> assert false in  (* is_channel みたいなのほしい *)
            fundefs @ [(function_name t, new_args, List.fold_left (fun expr (env,ys,e) -> Choice(expr, close env (subst_expr (M.of_list2 ys (List.map (fun z -> Var(z)) new_args)) e))) Skip l)])
        []
        f

let makeprog (e,f) = (makefunc f, e)


let transform p = 
    let e,f = trans !RefinementType.extenv p in
    makeprog (e,f)


    (* Bind, Val *)