open PiSyntax

(** syntax of expressions **)
type expr = 
    | Skip 
    | Let of string * value * expr
    | LetNonDet of string * expr
    | Call of string * value list
    | Choice of expr * expr
    | If of value * expr * expr
    | Assume of value * expr

(** syntax of function definitions **)
type fundef = string * string list * expr

(** syntax of sets of function definitions **)
type fundefs = fundef list

(** syntax of sequential programs **)
type prog = fundefs * expr




(* main関数内の自由変数をなくすために *)
(* 本当は型環境extenvを見たほうが良いかもしれませんが *)
let rec fv_val = function
    | Var(x) -> S.singleton x
    | Bool(_) | Int(_) -> S.empty
    | Op(_,vs) | Unknown(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs
    | Exists(xs,v) -> S.diff (fv_val v) (S.of_list xs)
let rec fv_expr = function  (* bool or int *)
    | Skip -> S.empty
    | Let(x,v,e) -> S.union (fv_val v) (S.remove x (fv_expr e))
    | LetNonDet(x,e) -> S.remove x (fv_expr e)
    | Call(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs
    | Choice(e1,e2) -> S.union (fv_expr e1) (fv_expr e2)
    | If(v,e1,e2) -> S.union (S.union (fv_val v) (fv_expr e1)) (fv_expr e2)
    | Assume(v,e) -> S.union (fv_val v) (fv_expr e)
let close_main e = 
    let fv = fv_expr e in
    S.fold_right (fun x e -> LetNonDet(x, e)) fv e




(** pretty-print **)
open Format
open Utilities
open PiSyntax

(* 以下はC用に!!!! *)

let nondet = "nondet()"

let rec pp_print_expr ppf = function
    | Skip -> pp_print_string ppf "return 0;"
    | Let(x,v,e) -> 
        fprintf ppf "@[<v 0>int %s = %a;@ %a@]" x pp_print_val v pp_print_expr e 
    | LetNonDet(x,e) ->
        fprintf ppf "@[<v 0>int %s = %s;@ %a@]" x nondet pp_print_expr e 
    | Call(x,vs) ->
        fprintf ppf "@[<h 0>%s%a;@]" x (pp_print_list pp_print_val) vs
    | Choice(e1,e2) -> 
        fprintf ppf "@[<v 0>if (%s) {@;<1 4>%a@ } else {@;<1 4>%a@ }@]" nondet pp_print_expr e1 pp_print_expr e2
    | If(v,e1,e2) -> 
        fprintf ppf "@[<v 0>if (%a) {@;<1 4>%a@ } else {@;<1 4>%a@ }@]" pp_print_val v pp_print_expr e1 pp_print_expr e2
    | Assume(v,e) -> 
        fprintf ppf "@[<v 0>if (%a) {@;<1 4>%a@ } else {@;<1 4>return 0;@ }@]" pp_print_val v pp_print_expr e
(* value @[<h 0>]を指定 *)
(* valueのpred *)


let add_int xs = List.map (fun x -> ("int", x)) xs

let pp_print_prototype_fundecl ppf (f,ys,_) =
    fprintf ppf "@[int %s%a;@]" f (pp_print_list (pp_print_pair ~left:"" ~right:"" ~delimiter:"" pp_print_string pp_print_string)) (add_int ys)

let pp_print_fundef ppf (f,ys,e) =
    fprintf ppf "@[int %s%a {@  %a@ }@]" f (pp_print_list (pp_print_pair ~left:"" ~right:"" ~delimiter:"" pp_print_string pp_print_string)) (add_int ys) pp_print_expr e

let pp_print_mainfundef ppf e =
    fprintf ppf "@[int main() {@ %a@ }@]" pp_print_expr (close_main e) 

let pp_print_prog ppf (fundefs,e) =
    fprintf ppf "@[#define true 1\n#define false 0\nint nondet() { int n; return n; }@ %a@ %a@ %a@]" 
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_prototype_fundecl) fundefs
        (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_fundef) fundefs 
        pp_print_mainfundef e

let print_prog oc prog = fprintf (formatter_of_out_channel oc) "%a@." pp_print_prog prog


let rec subst_expr map = function
    | Skip -> Skip
    | Let(x,v,e) -> Let(x, subst_val map v, subst_expr (M.remove x map) e)
    | LetNonDet(x,e) -> LetNonDet(x, subst_expr (M.remove x map) e)
    | Call(f,vs) -> Call(f, List.map (subst_val map) vs)
    | Choice(e1,e2) -> Choice(subst_expr map e1, subst_expr map e2)
    | If(v,e1,e2) -> If(subst_val map v, subst_expr map e1, subst_expr map e2)
    | Assume(v,e) -> Assume(subst_val map v, subst_expr map e)
