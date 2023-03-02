(** operators **)
type op = NOT | AND | OR | IMPLY | EQ | LT | GT | LE | GE | MINUS | ADD | SUB | MUL | DIV

(** syntax of values **)
type value =
    | Var of string
    | Bool of bool
    | Int of int
    | Op of op * value list

(** syntax of processes **)
(** the parameter 'a denotes the type of type expressions for bound variables **)
type 'a proc =
    | Nil
    | Nu of string * 'a * 'a proc
    | In of string * (string * 'a) list * 'a proc
    | RIn of string * (string * 'a) list * 'a proc
    | Out of string * value list * 'a proc
    | Par of 'a proc list
    | If of value * 'a proc * 'a proc


(** pretty-print **)
open Format
open Utilities

(*
let op_name = function
    | NOT -> "not"
    | AND -> "and"
    | OR -> "or"
    | IMPLY -> "->"
    | EQ -> "="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="
    | MINUS -> "-"
    | ADD -> "+"
    | SUB -> "-"
    | MUL -> "*"
    | DIV -> "/"

let val_name = function
    | Var(_) -> "var"
    | Bool(_) -> "bool"
    | Int(_) -> "int"
    | Op(op,_) -> op_name op
    | Unknown(_) -> "unknown"
*)

let prec_or = 1
let prec_and = 2
let prec_eq = 3
let prec_addsub = 4
let prec_muldiv = 5
let prec_minus = 6
let prec_not = 7

(* let rec pr_val prec ppf = function
    | Var(x) -> pp_print_string ppf x
    | Bool(b) -> pp_print_bool ppf b
    | Int(i) -> pp_print_int ppf i
    | Op(NOT,[v]) -> 
        if prec > prec_not then pp_print_string ppf "(";
        fprintf ppf "not %a" pp_print_val v;
        if prec > prec_not then pp_print_string ppf ")";
    | Op(MINUS,[v]) -> 

    | Op(OR,[v1;v2]) ->
        if prec > prec_or then  *)


(* 
let rec pp_print_val ppf v =
    match v with
    | Var(x) -> pp_print_string ppf x
    | Int(i) -> pp_print_int ppf i
    | Op(NOT,[v]) -> fprintf ppf "not %a" pp_print_val v 
    | Op(MINUS,[v]) -> fprintf ppf "- %a" pp_print_val v 
    | Op(AND,[v1;v2]) | Op(OR,[v1;v2]) 
    | Op(EQ,[v1;v2]) | Op(LT,[v1;v2]) | Op(GT,[v1;v2]) | Op(LE,[v1;v2]) | Op(GE,[v1;v2]) 
    | Op(ADD,[v1;v2]) | Op(SUB,[v1;v2]) | Op(MUL,[v1;v2]) | Op(DIV,[v1;v2]) ->
        fprintf ppf "%a %s %a" pp_print_val v1 (val_name v) pp_print_val v2
    | Unknown(x,vs) -> 
        fprintf ppf "(%s %a)" x (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_val) vs
    | _ -> assert false 
*)

let op_name = function  (* C用にしているがご愛顧 *)
    | NOT -> "!"
    | AND -> "&&"
    | OR -> "||"
    | IMPLY -> "IMPLY"  (* imply別にしても良さそう *)
    | EQ -> "=="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="
    | MINUS -> "-"  (* これを利用したとき、スペースのせいで失敗しないか *)
    | ADD -> "+"
    | SUB -> "-"
    | MUL -> "*"
    | DIV -> "/"

(* 
let rec pp_print_val ppf = function
    | Var(x) -> pp_print_string ppf x
    | Bool(b) -> pp_print_bool ppf b
    | Int(i) -> pp_print_int ppf i
    | Op(op,vs) -> 
        fprintf ppf "(@[%s@ %a@])" (op_name op) (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_val) vs
    | Unknown(phi,vs) ->
        fprintf ppf "(@[%s@ %a@])" phi (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_val) vs 
*)

(* let string_of_val v = fprintf str_formatter "%a" pp_print_val v *)


let rec pp_print_val ppf = function
    | Var(x) -> pp_print_string ppf x
    | Bool(b) -> pp_print_bool ppf b
    | Int(i) -> 
        if i >= 0 then pp_print_int ppf i    (* 本当はi<0で区別すべき?? *)
        else fprintf ppf "(%d)" i
    | Op(op,vs) -> 
        (match vs with
        | [v] -> fprintf ppf "(@[%s@ %a@])" (op_name op) pp_print_val v
        | vs -> (pp_print_list ~left:"(" ~right:")" ~delimiter:(" "^op_name op) pp_print_val) ppf vs)

let print_val oc v = 
    fprintf (formatter_of_out_channel oc) "%a@." pp_print_val v


let prec_nu = 1
let prec_par = 2
let prec_if = 3
let prec_io = 4

let rec pr_proc ?pp_print_t prec ppf = 
    let pr_proc prec ppf p = match pp_print_t with None -> pr_proc prec ppf p | Some(pp_print_t) -> pr_proc ~pp_print_t:pp_print_t prec ppf p in
    function
    | Nil -> pp_print_string ppf "O"
    | Nu(x,t,p) -> 
        if prec > prec_nu then pp_print_string ppf "(";
        (match pp_print_t with
        | None             -> fprintf ppf "@[new %s in@ %a@]"                                             x    (pr_proc prec_nu) p
        | Some(pp_print_t) -> fprintf ppf "@[new %a in@ %a@]" (pp_print_pair pp_print_string pp_print_t) (x,t) (pr_proc prec_nu) p);
        if prec > prec_nu then pp_print_string ppf ")"
    | In(x,yts,Nil) -> 
        (match pp_print_t with
        | None             -> fprintf ppf "@[%s?%a@]" x (pp_print_list                pp_print_string            ) (List.map fst yts) 
        | Some(pp_print_t) -> fprintf ppf "@[%s?%a@]" x (pp_print_list (pp_print_pair pp_print_string pp_print_t))               yts )
    | In(x,yts,p) -> 
        (match pp_print_t with
        | None             -> fprintf ppf "@[%s?%a.@ %a@]" x (pp_print_list                pp_print_string            ) (List.map fst yts) (pr_proc prec_io) p
        | Some(pp_print_t) -> fprintf ppf "@[%s?%a.@ %a@]" x (pp_print_list (pp_print_pair pp_print_string pp_print_t))               yts  (pr_proc prec_io) p)
    | RIn(x,yts,Nil) -> 
        (match pp_print_t with
        | None             -> fprintf ppf "@[*%s?%a@]" x (pp_print_list                pp_print_string            ) (List.map fst yts) 
        | Some(pp_print_t) -> fprintf ppf "@[*%s?%a@]" x (pp_print_list (pp_print_pair pp_print_string pp_print_t))               yts )
    | RIn(x,yts,p) -> 
        (match pp_print_t with
        | None             -> fprintf ppf "@[*%s?%a.@ %a@]" x (pp_print_list                pp_print_string             ) (List.map fst yts) (pr_proc prec_io) p
        | Some(pp_print_t) -> fprintf ppf "@[*%s?%a.@ %a@]" x (pp_print_list (pp_print_pair pp_print_string pp_print_t))                yts  (pr_proc prec_io) p)
    | Out(x,vs,Nil) ->
        fprintf ppf "@[%s!%a@]" x (pp_print_list pp_print_val) vs
    | Out(x,vs,p) -> 
        fprintf ppf "@[%s!%a.@ %a@]" x (pp_print_list pp_print_val) vs (pr_proc prec_io) p
    | Par(ps) -> 
        (* 優先順位を確認 *)
        if prec > prec_par then pp_print_string ppf "(";
        fprintf ppf "@[%a@]" (pp_print_list ~left:"" ~right:"" ~delimiter:" | " (pr_proc prec_par)) ps;
        if prec > prec_par then pp_print_string ppf ")"
    | If(v,p1,p2) -> 
        if prec > prec_if then pp_print_string ppf "(";
        fprintf ppf "@[if %a@ then %a@ else %a@]" pp_print_val v (pr_proc prec_if) p1 (pr_proc prec_if) p2;
        if prec > prec_if then pp_print_string ppf ")"

let pp_print_proc ?pp_print_t ppf p = 
    let pr_proc prec ppf p = match pp_print_t with None -> pr_proc prec ppf p | Some(pp_print_t) -> pr_proc ~pp_print_t:pp_print_t prec ppf p in
    let prec = 0 in
    pr_proc prec ppf p 

let print_proc ?pp_print_t oc p = 
    let pp_print_proc ppf p = match pp_print_t with None -> pp_print_proc ppf p | Some(pp_print_t) -> pp_print_proc ~pp_print_t:pp_print_t ppf p in
    fprintf (formatter_of_out_channel oc) "%a@." pp_print_proc p





(* 
let p = 
    RIn("fact", [("n",()); ("r",())], 
        If(Var("n"), Out("r", [Int(1)], Nil),
                     Nu("r'", (), Out("r'", [Var("n")], Nil))))

let _ = print_proc stdout p 
*)


let rec subst_val map = function
    | Var(x) -> (try M.find x map with Not_found -> Var(x))
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | Op(op,vs) -> Op(op, List.map (subst_val map) vs)

let subst_var map x = 
    try (match M.find x map with Var(y) -> y | _ -> failwith "subst channel")
    with Not_found -> x

let rec subst_proc map = function
    | Nil -> Nil
    | Nu(x,t,p) -> Nu(x, t, subst_proc (M.remove x map) p)
    | In(x,yts,p) -> In(subst_var map x, yts, subst_proc (M.remove_list (List.map fst yts) map) p)
    | RIn(x,yts,p) -> RIn(subst_var map x, yts, subst_proc (M.remove_list (List.map fst yts) map) p)
    | Out(x,vs,p) -> Out(subst_var map x, List.map (subst_val map) vs, subst_proc map p)
    | Par(ps) -> Par(List.map (subst_proc map) ps)
    | If(v,p1,p2) -> If(subst_val map v, subst_proc map p1, subst_proc map p2)


let rec fv_val = function
    | Var(x) -> S.singleton x
    | Bool(_) | Int(_) -> S.empty
    | Op(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs

let rec fv_proc = function
    | Nil -> S.empty
    | Nu(x,t,p) -> S.remove x (fv_proc p)
    | In(x,yts,p) -> S.add x (S.remove_list (List.map fst yts) (fv_proc p))
    | RIn(x,yts,p) -> S.add x (S.remove_list (List.map fst yts) (fv_proc p))
    | Out(x,vs,p) -> S.union (S.add x (List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs)) (fv_proc p)
    | Par(ps) -> List.fold_left (fun set p -> S.union set (fv_proc p)) S.empty ps
    | If(v,p1,p2) -> S.union (S.union (fv_val v) (fv_proc p1)) (fv_proc p2)
