open PiSyntax
open SimpleType
open SeqSyntax

let is_bool_int_or_chstar = function
    | SCh(_,i) when i >= 0 -> false
    | _ -> true


let function_name = function
    | SCh(_,i) -> "function" ^ string_of_int i
    | _ -> assert false


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
let empty_chenv (chenv : (string, SimpleType.t) M.t) =
    (* ch-type \mapsto \epsilon  *)
    let img = M.fold_left (fun set _ t -> S.add t set) S.empty chenv in
    S.fold_left (fun fundefs -> function SCh(ts,i) as t when i>=0 -> fundefs @ [(function_name t, new_args (List.filter is_bool_int_or_chstar ts), Skip)] | _ -> assert false) [] img

let rec trans bienv chenv = function
    | Nil -> (empty_chenv chenv, Skip)
    | Nu(x,t,p) -> trans bienv (M.add x t chenv) p
    | In(_,bindings,p) ->
        let (yts,zts) = List.partition (fun (_,t) -> is_bool_or_int t) bindings in
        let (fundefs,e) = trans (M.add_list yts bienv) (M.add_list zts chenv) p in 
        (fundefs, List.fold_right (fun (y,_) e -> LetNonDet(y, e)) yts e)
    | RIn(x,bindings,p) -> 
        let t = type_of bienv chenv (Var(x)) in
        let (yts,zts) = List.partition (fun (_,t) -> is_bool_int_or_chstar t) bindings in
        let (fundefs,e) = trans (M.add_list yts bienv) (M.add_list zts chenv) p in
        ([(function_name t, List.map fst yts, e)] + fundefs, Skip)
    | Out(x,vs,p) -> 
        let t = type_of bienv chenv (Var(x)) in
        if is_bool_int_or_chstar t then
            let vs' = List.filter (fun v -> is_bool_int_or_chstar (type_of bienv chenv v)) vs in
            let (fundefs,e) = trans bienv chenv p in
            (fundefs, Choice(Call(x, vs), e))  (* 臨時 *)
        else
            let vs' = List.filter (fun v -> is_bool_int_or_chstar (type_of bienv chenv v)) vs in
            let (fundefs,e) = trans bienv chenv p in
            (fundefs, Choice(Call(function_name t, vs), e))  (* 臨時 *)
    | Par(p1,p2) -> 
        let (fundefs1,e1) = trans bienv chenv p1 in
        let (fundefs2,e2) = trans bienv chenv p2 in
        (fundefs1 + fundefs2, Choice(e1, e2))
    | If(v,p1,p2) -> 
        let (fundefs1,e1) = trans bienv chenv p1 in
        let (fundefs2,e2) = trans bienv chenv p2 in
        (fundefs1 + fundefs2, If(v, e1, e2))


let close_expr ys e = 
    let zs = S.diff (fv_expr e) (S.of_list ys) in
    S.fold_right (fun z e -> LetNonDet(z, e)) zs e
let close_fundef (f,ys,e) = (f, ys, close_expr ys e)
let close_fundefs fundefs = List.map close_fundef fundefs
let close_prog (fundefs,e) = (close_fundefs fundefs, close_expr [] e)


let transform p = 
    simplify_prog 
    (* (close_prog  *)
    (trans !ext_bienv M.empty p)
    (* ) *)
