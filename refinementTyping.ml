open PiSyntax
open RefinementType

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



let vars = ref M.empty    (* assert (forall (x ここの型を知りたい)) のため *)
let predvars = ref M.empty


let num = ref 0
let new_var t =
    incr num;
    let ret = "_v" ^ string_of_int !num in
    (* adhoc *) vars := M.add ret t !vars;
    ret
let new_predvar bienv =
    incr num;
    let phi = "_p" ^ string_of_int !num in
    let ys,ts = List.split (M.to_list bienv) in
    predvars := M.add phi ts !predvars;
    Unknown(phi, List.map (fun y -> Var(y)) ys)


let rec new_t bienv = function
    | SimpleType.SBool -> RBool
    | SimpleType.SInt -> RInt
    | SimpleType.SCh(ts,i) ->
        let ts1,ts2 = List.partition SimpleType.is_bool_or_int ts in
        let yts = List.map (fun t -> let t' = new_t bienv t in (new_var t', t')) ts1 in
        let vI = new_predvar (M.add_list yts bienv) in
        let ts2I = List.map (new_t (M.add_list yts bienv)) ts2 in
        let vO = new_predvar (M.add_list yts bienv) in
        let ts2O = List.map (new_t (M.add_list yts bienv)) ts2 in
        RCh(yts, vI, ts2I, vO, ts2O, i)

(* 
let predicates env = 
    List.fold_left 
        (fun value el -> 
            match el with
            | Bind(x,RInt(y,v)) -> Op(AND, [value; subst_val (M.singleton y (Var(x))) v])
            | Bind(x,RCh(_)) -> value
            | Val(v) -> Op(AND, [value; v]))
        (Bool(true))
        env    
*)
  
let rec subtype bienv prenv t t' =
    match t, t' with
    | RCh(yts,vI,tsI,vO,tsO,_), RCh(yts',vI',tsI',vO',tsO',_) -> 
        let sigma = M.of_list (List.map2 (fun (y,_) (y',_) -> (y', Var(y))) yts yts') in
        let vI' = subst_val sigma vI' in
        let tsI' = List.map (subst_t sigma) tsI' in
        let vO' = subst_val sigma vO' in
        let tsO' = List.map (subst_t sigma) tsO' in
          [(prenv @ [vI ], vI')] @ List.concat (List.map2 (subtype (M.add_list yts bienv) (prenv @ [vI ])) tsI  tsI')  
        @ [(prenv @ [vO'], vO )] @ List.concat (List.map2 (subtype (M.add_list yts bienv) (prenv @ [vO'])) tsO' tsO )
    | _ -> assert false

(* let infer_val env = function
    | Var(x) as v -> 
        let t = find x env in
        (match t with RCh(_) -> t | RInt(_) ->
        let x = new_var () in 
        RInt(x, Op(EQ, [Var(x); v]))
        )
    | Bool(_) | Int(_) as v -> 
        let x = new_var () in 
        RInt(x, Op(EQ, [Var(x); v]))
    | Op(_) as v ->  (* bool, ch *)
        let x = new_var () in 
        RInt(x, Op(EQ, [Var(x); v]))
    | Unknown(_) -> failwith "kangaeyou" *)


(* let rec fvint_env = function
    | [] -> []
    | Bind(x,RInt(_))::env -> x :: List.filter ((<>) x) (fvint_env env)
    | _::env -> fvint_env env *)

let rec infer_proc bienv prenv chenv = function
    | Nil -> Nil, []
    | Nu(x,t,p) ->  (* closedなプロセスを仮定 *)
        let t' = new_t bienv t in
        (match t' with RCh(yts,vI,tsI,vO,tsO,_) -> 
        let p',c = infer_proc bienv prenv (M.add x t' chenv) p in
        Nu(x, t', p'), [(prenv @ [vO], vI)] @ List.concat (List.map2 (subtype (M.add_list yts bienv) (prenv @ [vO])) tsO tsI) @ c
        | _ -> assert false)
    | In(x,bindings,p) -> 
        let t = type_of bienv chenv (Var(x)) in 
        let yts,zts = List.partition (fun (_,t) -> SimpleType.is_bool_or_int t) bindings in
        (match t with RCh(yts',vI,tsI,_,_,_) -> 
        let sigma = M.of_list (List.map2 (fun (y',_) (y,_) -> (y', Var(y))) yts' yts) in
        let p',c = infer_proc (M.add_list (List.map2 (fun (y,_) (_,t) -> (y,t)) yts yts') bienv)
                              (prenv @ [subst_val sigma vI]) 
                              (M.add_list (List.map2 (fun (z,_) t -> (z, subst_t sigma t)) zts tsI) chenv) 
                              p in
        (* adhoc *) vars := M.add_list (List.map2 (fun (y,_) (_,t) -> (y,t)) yts yts') !vars;
        In(x, (List.map2 (fun (y,_) (_,t) -> (y,t)) yts yts') @ (List.map2 (fun (z,_) t -> (z, subst_t sigma t)) zts tsI), p'), c
        | _ -> assert false)
    | RIn(x,bindings,p) -> 
        let t = type_of bienv chenv (Var(x)) in 
        let yts,zts = List.partition (fun (_,t) -> SimpleType.is_bool_or_int t) bindings in
        (match t with RCh(yts',vI,tsI,_,_,_) -> 
        let sigma = M.of_list (List.map2 (fun (y',_) (y,_) -> (y', Var(y))) yts' yts) in
        let p',c = infer_proc (M.add_list (List.map2 (fun (y,_) (_,t) -> (y,t)) yts yts') bienv)
                              (prenv @ [subst_val sigma vI]) 
                              (M.add_list (List.map2 (fun (z,_) t -> (z, subst_t sigma t)) zts tsI) chenv) 
                              p in
        (* adhoc *) vars := M.add_list (List.map2 (fun (y,_) (_,t) -> (y,t)) yts yts') !vars;
        RIn(x, (List.map2 (fun (y,_) (_,t) -> (y,t)) yts yts') @ (List.map2 (fun (z,_) t -> (z, subst_t sigma t)) zts tsI), p'), c
        | _ -> assert false)
    | Out(x,vs,p) -> 
        let t = type_of bienv chenv (Var(x)) in
        let vs1,vs2 = List.partition (fun v -> is_bool_or_int (type_of bienv chenv v)) vs in
        (match t with RCh(yts',_,_,vO,tsO,_) ->  (* match of_type x env with *)
        let ts2 = List.map (type_of bienv chenv) vs2 in
        (* ts are sub-types of []zts *)
        let p',c = infer_proc bienv prenv chenv p in
        if List.length yts' <> List.length vs1 then assert false;
        if List.length ts2 <> List.length tsO then assert false;
        let sigma = M.of_list (List.map2 (fun (y',_) v -> (y',v)) yts' vs1) in
        Out(x,vs,p'), (prenv, subst_val sigma vO) :: List.concat (List.map2 (subtype bienv prenv) ts2 (List.map (subst_t sigma) tsO)) @ c
        | _ -> assert false)
    | Par(p1,p2) -> 
        let p1',c1 = infer_proc bienv prenv chenv p1 in
        let p2',c2 = infer_proc bienv prenv chenv p2 in
        Par(p1', p2'), c1 @ c2
    | If(v,p1,p2) -> 
        let p1',c1 = infer_proc bienv (prenv @ [        v  ]) chenv p1 in
        let p2',c2 = infer_proc bienv (prenv @ [Op(NOT,[v])]) chenv p2 in
        If(v, p1', p2'), c1 @ c2


let to_chc (prenv, v) = 
    let guard = match prenv with
    | [] -> Bool(true)
    | [v] -> v
    | vs -> Op(AND, vs)
    in Op(IMPLY, [guard; v])

let typing p = 
    ext_bienv := M.map (new_t M.empty) !SimpleType.ext_bienv;
    (* adhoc *) vars := !ext_bienv;
    let p',c = infer_proc !ext_bienv [] M.empty p in
    (p', List.map to_chc c)
    



(** smt2 **)
open Format
open Utilities

let op_name = function
    | NOT -> "not"
    | AND -> "and"
    | OR -> "or"
    | IMPLY -> "=>"
    | EQ -> "="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="
    | MINUS -> "-"  (* これを利用したとき、スペースのせいで失敗しないか *)
    | ADD -> "+"
    | SUB -> "-"
    | MUL -> "*"
    | DIV -> "/"

let rec pp_print_val_for_smt2 ppf = function
    | Var(x) -> pp_print_string ppf x
    | Bool(b) -> pp_print_bool ppf b
    | Int(i) -> pp_print_int ppf i
    | Op(op,vs) -> 
        fprintf ppf "(@[%s@ %a@])" (op_name op) (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_val_for_smt2) vs
    | Unknown(x,vs) ->
        fprintf ppf "(@[%s@ %a@])" x (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_val_for_smt2) vs


(* 
let rec pvars_proc = function
    | Nil -> S.empty
    | Nu(_,_,p) | In(_,_,p) | RIn(_,_,p) | Out(_,_,p) -> pvars_proc p
    | Par(p1,p2) -> pvars_proc p1  *)



let rec fv_val = function
    | Var(x) -> S.singleton x
    | Bool(_) | Int(_) -> S.empty
    | Op(_,vs) | Unknown(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs

let pp_print_bi ppf = function
    | RBool -> pp_print_string ppf "Bool"
    | RInt -> pp_print_string ppf "Int"
    | _ -> failwith "channnnnnnnnnnnnnnnel"

let pp_print_declfun ppf (phi,ts) =  (* iだけじゃだめ、intじゃなくてboolの可能性も *)
    fprintf ppf "(@[declare-fun@ %s@ %a@ Bool@])" 
        phi 
        (pp_print_list ~left:"(" ~right:")" ~delimiter:"" pp_print_bi) ts  


let pp_print_assert ppf v = 
    let fv = S.to_list (fv_val v) in
    let rec gen_list = function [] -> [] | x::xs -> 
        (try (x, M.find x !vars) :: gen_list xs with Not_found -> 
             (x, RInt)           :: gen_list xs) in    (* 嘘、後で直すこと。boolなら失敗 *)
    (* if fv <> [] then *)
    fprintf ppf "(@[assert (@[forall (%a)@ %a@])@])"
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" (pp_print_pair ~left:"(" ~right:")" ~delimiter:"" pp_print_string pp_print_bi)) (gen_list fv)
        pp_print_val_for_smt2 v
    (* else
    fprintf ppf "(@[assert @[%a@]@])"
        pp_print_val_for_smt2 v *)

let pp_print_smt2 ppf chc =
    fprintf ppf "@[<v 0>(set-logic HORN)@ %a@ %a@ (check-sat)@ (get-model)@]" 
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_declfun) (M.to_list !predvars)
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_assert) chc                                          

let print_smt2 oc chc = 
    fprintf (formatter_of_out_channel oc) "%a@." pp_print_smt2 chc
 