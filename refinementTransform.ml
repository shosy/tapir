open PiSyntax
open SimpleType
open RefinementType
open SeqSyntax




(* refinementTypingにも記載 *)
(* let rec add el = function
    | [] -> [el]
    | hd::tl -> 
        (match el, hd with
        | Bind(x,_), Bind(y,_) when x = y -> add el tl
        | _ -> hd :: add el tl)
let rec find x = function
    | [] -> raise Not_found
    | Bind(y,t)::_ when y = x -> t
    | _::env -> find x env
let add_list xts env = List.fold_left (fun env (x,t) -> add (Bind(x,t)) env) env xts  *)


(* let num0 = ref 0
let new_var () =
    incr num0;
    "_v" ^ string_of_int !num0 *)




let function_name = function
    | RCh(_,_,_,_,_,i) -> "function" ^ string_of_int i
    | _ -> assert false


(* let rec simple = function
    | RInt(_) -> SInt
    | RCh(zts,i) -> SCh(List.map (fun (_,t) -> simple t) zts, i) *)


(* let (+) (f1 : (SimpleType.t, (rTypeEnv * string list * SeqSyntax.expr) list) M.t) f2 =
    M.merge
        (fun t tmp1 tmp2 -> 
            match tmp1, tmp2 with
            | Some(l1), Some(l2) -> Some(l1 @ l2)
            | Some(l1), None -> Some(l1)
            | None, Some(l2) -> Some(l2)
            | _ -> None)
        f1
        f2 *)
let (+) (fundefs1 : fundefs) (fundefs2 : fundefs) : fundefs =
    let m = M.merge
        (fun f tmp1 tmp2 -> 
            match tmp1, tmp2 with
            | Some(ys1,e1), Some(ys2,e2) -> Some(ys1, Choice(e1, subst_expr (M.of_list2 ys2 (List.map (fun y -> Var(y)) ys1)) e2))
            | Some(ys,e), None -> Some(ys, e)
            | None, Some(ys,e) -> Some(ys, e)
            | _ -> None)
        (M.of_list (List.map (fun (f,ys,e) -> (f,(ys,e))) fundefs1))
        (M.of_list (List.map (fun (f,ys,e) -> (f,(ys,e))) fundefs2))
    in List.map (fun (f,(ys,e)) -> (f,ys,e)) (M.to_list m)

let new_args = 
    let num = ref 0 in 
    fun xs -> List.map (fun _ -> "arg" ^ string_of_int (incr num; !num)) xs
let empty_chenv chenv =
    (* ch-type \mapsto \epsilon  *)
    let img = M.fold_left (fun set _ t -> S.add t set) S.empty chenv in
    S.fold_left (fun fundefs -> function RCh(yts,_,_,_,_,i) as t -> fundefs @ [(function_name t, new_args yts, Skip)] | _ -> assert false) [] img
    
let rec trans bienv chenv = function
    | Nil -> (empty_chenv chenv, Skip)
    | Nu(x,t,p) -> trans bienv (M.add x t chenv) p
    | In(x,bindings,p) ->
        (match type_of bienv chenv (Var(x)) with RCh(yts',vI,_,_,_,_) -> 
        let (yts,zts) = List.partition (fun (_,t) -> is_bool_or_int t) bindings in
        let (fdefs,e) = trans (M.add_list yts bienv) (M.add_list zts chenv) p in 
        let sigma = M.of_list (List.map2 (fun (y',_) (y,_) -> (y', Var(y))) yts' yts) in  (* difference from simpletype *)
        let vI = subst_val sigma vI in
        (fdefs, List.fold_right (fun (y,_) e -> LetNonDet(y, e)) yts (Assume(vI, e)))  
        | _ -> assert false)
    | RIn(x,bindings,p) ->
        let t = type_of bienv chenv (Var(x)) in
        let (yts,zts) = List.partition (fun (_,t) -> is_bool_or_int t) bindings in
        let (fdefs,e) = trans (M.add_list yts bienv) (M.add_list zts chenv) p in
        ([(function_name t, List.map fst yts, e)] + fdefs, Skip)
    | Out(x,vs,p) ->
        let t = type_of bienv chenv (Var(x)) in
        let vs' = List.filter (fun v -> is_bool_or_int (type_of bienv chenv v)) vs in
        let (fdefs,e) = trans bienv chenv p in
        (fdefs, Choice(Call(function_name t, vs'), e))
    | Par(p1,p2) -> 
        let (fdefs1,e1) = trans bienv chenv p1 in
        let (fdefs2,e2) = trans bienv chenv p2 in
        (fdefs1 + fdefs2, Choice(e1, e2))
    | If(v,p1,p2) -> 
        let (fdefs1,e1) = trans bienv chenv p1 in
        let (fdefs2,e2) = trans bienv chenv p2 in
        (fdefs1 + fdefs2, If(v, e1, e2))
        

(* let close env e = 
    List.fold_right 
        (fun el e -> match el with   Bind(x,RInt(y,v)) -> LetNonDet(x, Assume(subst_val (M.singleton y (Var(x))) v, e)) 
                                   | Bind(x,RCh(_)) -> e
                                   | Val(v) -> Assume(v, e))
        env
        e  *)

(* let num = ref 0
let makefunc (f : (SimpleType.t, (rTypeEnv * string list * SeqSyntax.expr) list) M.t) =
    M.fold_left
        (fun fundefs t l -> 
            let new_args = match t with SCh(ts,_) -> List.map (fun _ -> incr num; "arg" ^ string_of_int !num) (List.filter ((=) SInt) ts) | _ -> assert false in  (* is_channel みたいなのほしい *)
            fundefs @ [(function_name t, new_args, List.fold_left (fun expr (env,ys,e) -> Choice(expr, close env (subst_expr (M.of_list2 ys (List.map (fun z -> Var(z)) new_args)) e))) Skip l)])
        []
        f

let makeprog (e,f) = (makefunc f, e) *)

let close_expr ys e = 
    let zs = S.diff (fv_expr e) (S.of_list ys) in
    S.fold_right (fun z e -> LetNonDet(z, e)) zs e
let close_fundef (f,ys,e) = (f, ys, close_expr ys e)
let close_fundefs fundefs = List.map close_fundef fundefs
let close_prog (fundefs,e) = (close_fundefs fundefs, close_expr [] e)


let transform p = 
    (* let e,f = trans !RefinementType.extenv p in
    makeprog (e,f) *)
    close_prog (trans !ext_bienv M.empty p)


    (* Bind, Val *)