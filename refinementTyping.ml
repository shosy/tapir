open PiSyntax
open RefinementType

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




let predvars = ref M.empty


let num = ref 0
let new_var () =
    incr num;
    "_v" ^ string_of_int !num
let new_predvar xs =
    incr num;
    let phi = "_p" ^ string_of_int !num in
    predvars := M.add phi (List.length(xs)) !predvars;
    Unknown(phi, List.map (fun x -> Var(x)) xs)


let rec new_t intfreevars = function
    | SimpleType.SInt ->
        let x = new_var () in
        let v = new_predvar (intfreevars @ [x]) in
        RInt(x, v)
    | SimpleType.SCh(ts,i) ->
        let new_args = List.map (fun _ -> new_var ()) ts in
        let (zts,_) = List.fold_left2 
            (fun (zts,intfreevars) x t -> (zts @ [(x, new_t intfreevars t)], match t with SInt | SBool -> intfreevars@[x] | SCh(_) -> intfreevars))
            ([], intfreevars)     
            new_args
            ts
        in RCh(zts,i)
    | _ -> failwith "boooooooool"


let predicates env = 
    List.fold_left 
        (fun value el -> 
            match el with
            | Bind(x,RInt(y,v)) -> Op(AND, [value; subst_val (M.singleton y (Var(x))) v])
            | Bind(x,RCh(_)) -> value
            | Val(v) -> Op(AND, [value; v]))
        (Bool(true))
        env   
    
let rec subtype env t1 t2 = 
    match t1, t2 with
    | RInt(x,v1), RInt(y,v2) -> 
        [Op(IMPLY, [Op(AND, [predicates env; v1]); 
                    subst_val (M.singleton y (Var(x))) v2])]
    | RCh(xts,_), RCh(yts,_) -> 
        subtypes (add_list xts env) 
                 (List.map snd xts) 
                 (List.map (fun (y,t) -> subst_t (M.of_list2 (List.map fst yts) (List.map (fun (x,_) -> Var(x)) xts)) t) yts)
        @
        subtypes (add_list yts env) 
                 (List.map snd yts) 
                 (List.map (fun (x,t) -> subst_t (M.of_list2 (List.map fst xts) (List.map (fun (y,_) -> Var(y)) yts)) t) xts)
    | _ -> assert false
and subtypes env ts1 ts2 = 
    (* subtypes env [t1;t2] [s1;s2] 
      env |- t1 < s1,  env |- t2 < s2    envは一定 *)
    List.concat (List.map2 (fun t1 t2 -> subtype env t1 t2) ts1 ts2)

let infer_val env = function
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
    | Unknown(_) -> failwith "kangaeyou"


let rec fvint_env = function
    | [] -> []
    | Bind(x,RInt(_))::env -> x :: List.filter ((<>) x) (fvint_env env)
    | _::env -> fvint_env env

let rec infer_proc env = function
    | Nil -> Nil, []
    | Nu(x,t,p) -> 
        let t' = new_t (fvint_env env) t in
        let p',c = infer_proc (add (Bind(x,t')) env) p in
        Nu(x, t', p'), c
    | In(x,yts,p) -> 
        let t = find x env in (* match of_type x env with *)
        (match t with RCh(zts,_) -> 
        let yts' = List.map2 (fun (y,_) (z,t) -> (y, subst_t (M.of_list2 (List.map fst zts) (List.map (fun (y,_) -> Var(y)) yts)) t)) yts zts in  (* 厳密には違う *)
        let p',c = infer_proc (add_list yts' env) p in
        In(x, yts', p'), c
        | _ -> assert false)
    | RIn(x,yts,p) ->
        let t = find x env in  (* match of_type x env with *)
        (match t with RCh(zts,_) -> 
        let yts' = List.map2 (fun (y,_) (z,t) -> (y, subst_t (M.of_list2 (List.map fst zts) (List.map (fun (y,_) -> Var(y)) yts)) t)) yts zts in  (* 厳密には違う *)
        let p',c = infer_proc (add_list yts' env) p in
        RIn(x, yts', p'), c
        | _ -> assert false)
    | Out(x,vs,p) -> 
        let t = find x env in
        (match t with RCh(zts,_) ->  (* match of_type x env with *)
        let ts = List.map (infer_val env) vs in
        (* ts are sub-types of []zts *)
        let p',c = infer_proc env p in
        Out(x,vs,p'), subtypes env ts (List.map (fun (_,t) -> subst_t (M.of_list2 (List.map fst zts) vs) t) zts) @ c
        | _ -> assert false)
    | Par(p1,p2) -> 
        let p1',c1 = infer_proc env p1 in
        let p2',c2 = infer_proc env p2 in
        Par(p1', p2'), c1 @ c2
    | If(v,p1,p2) -> 
        let p1',c1 = infer_proc (add (Val(v)) env) p1 in
        let p2',c2 = infer_proc (add (Val(Op(NOT,[v]))) env) p2 in
        If(v, p1', p2'), c1 @ c2


(* 本当はargsortして、intを前に *)
let new_t2 = function
    | SimpleType.SInt -> RInt(new_var (), Bool(true))
    | SimpleType.SCh(_) as t -> new_t [] t  (* []???? *)
    | _ -> failwith "boooooooool"
let typing p = 
    extenv := M.fold_left (fun env x t -> add (Bind(x, (new_t2 t))) env) [] !SimpleType.extenv;
    infer_proc !extenv p
    



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

let pp_print_declfun ppf (phi,i) =  (* iだけじゃだめ、intじゃなくてboolの可能性も *)
    let rec gen_list i = if i = 0 then [] else "Int" :: gen_list (i-1) in
    fprintf ppf "(@[declare-fun@ %s@ %a@ Bool@])" 
        phi 
        (pp_print_list ~left:"(" ~right:")" ~delimiter:"" pp_print_string) (gen_list i)  


let pp_print_assert ppf v = 
    let fv = S.to_list (fv_val v) in
    let rec gen_list = function [] -> [] | x::xs -> (x, "Int") :: gen_list xs in
    fprintf ppf "(@[assert (@[forall (%a)@ %a@])@])"
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" (pp_print_pair ~left:"(" ~right:")" ~delimiter:"" pp_print_string pp_print_string)) (gen_list fv)
        pp_print_val_for_smt2 v

let pp_print_smt2 ppf chc =
    fprintf ppf "@[<v 0>(set-logic HORN)@ %a@ %a@ (check-sat)@ (get-model)@]" 
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_declfun) (M.to_list !predvars)
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_assert) chc                                          

let print_smt2 oc chc = 
    fprintf (formatter_of_out_channel oc) "%a@." pp_print_smt2 chc
 