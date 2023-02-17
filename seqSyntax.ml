open PiSyntax

(** syntax of expressions **)
type expr = 
    | Skip 
    | Let of string * value * expr
    | LetNonDet of string * expr
    | Call of string * value list
    | Choice of expr * expr
    | If of value * expr * expr

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
    | Op(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs
let rec fv_expr = function  (* bool or int *)
    | Skip -> S.empty
    | Let(x,v,e) -> S.union (fv_val v) (S.remove x (fv_expr e))
    | LetNonDet(x,e) -> S.remove x (fv_expr e)
    | Call(_,vs) -> List.fold_left (fun set v -> S.union set (fv_val v)) S.empty vs
    | Choice(e1,e2) -> S.union (fv_expr e1) (fv_expr e2)
    | If(v,e1,e2) -> S.union (S.union (fv_val v) (fv_expr e1)) (fv_expr e2)
let close_main e = 
    let fv = fv_expr e in
    S.fold_right (fun x e -> LetNonDet(x, e)) fv e




(** pretty-print **)
open Format
open Utilities
open PiSyntax

(* 以下はC用に!!!! *)

let nondetint = "Random.int 0"
let nondetbool = "Random.bool ()"

let rec print_expr = function
    | Skip -> print_string "()"
    | Let(x,v,e) -> 
        print_string "let ";
        print_string x;
        print_string " = ";
        print_newline ();
        print_val stdout v;
        print_string " in ";
        print_expr e 
    | LetNonDet(x,e) ->
        print_string "let ";
        print_string x;
        print_string " = ";
        print_string "Random.int 0";
        print_string " in ";
        print_expr e 
    | Call(x,vs) ->
        print_string x;
        print_string " ";
        if vs = [] then print_string "()" else List.iter (fun v -> print_newline (); print_val stdout v; print_string " ") vs
    | Choice(e1,e2) -> 
        print_string "if ";
        print_string "Random.bool ()";
        print_string " then (\n";
        print_expr e1;
        print_string ") else (\n";
        print_expr e2;
        print_string ")\n"
    | If(v,e1,e2) -> 
        print_string "if ";
        print_newline ();
        print_val stdout v;
        print_string " then (\n";
        print_expr e1;
        print_string ") else (\n";
        print_expr e2;
        print_string ")\n"
(* value @[<h 0>]を指定 *)
(* valueのpred *)


let add_int xs = List.map (fun x -> ("int", x)) xs

let pp_print_prototype_fundecl ppf (f,ys,_) =
    fprintf ppf "@[int %s%a;@]" f (pp_print_list (pp_print_pair ~left:"" ~right:"" ~delimiter:"" pp_print_string pp_print_string)) (add_int ys)

let print_first_fundef (f,ys,e) =
    print_string "let rec ";
    print_string f;
    print_string " ";
    if ys = [] then print_string "() " else List.iter (fun y -> print_string y; print_string " ") ys;
    print_string "= ";
    print_expr e

let print_rest_fundef (f,ys,e) =
    print_string "and ";
    print_string f;
    print_string " ";
    if ys = [] then print_string "() " else List.iter (fun y -> print_string y; print_string " ") ys;
    print_string "= ";
    print_expr e

let print_fundefs fundefs = 
    match fundefs with
    | [] -> ()
    | [fundef] -> 
        print_first_fundef fundef;
        print_string " in\n"
    | fundef::rest ->
        print_first_fundef fundef;
        List.iter print_rest_fundef rest;
        print_string " in\n"

let print_prog (fundefs,e) =
    print_fundefs fundefs;
    print_expr e


let substf map f = 
    try 
        (match M.find f map with Var(y) -> y | _ -> assert false) 
    with _ -> f
let rec subst_expr map = function
    | Skip -> Skip
    | Let(x,v,e) -> Let(x, subst_val map v, subst_expr (M.remove x map) e)
    | LetNonDet(x,e) -> LetNonDet(x, subst_expr (M.remove x map) e)
    | Call(f,vs) -> Call(substf map f, List.map (subst_val map) vs)
    | Choice(e1,e2) -> Choice(subst_expr map e1, subst_expr map e2)
    | If(v,e1,e2) -> If(subst_val map v, subst_expr map e1, subst_expr map e2)



let rec simplify_expr = function
    | Skip -> Skip
    | Let(x,v,e) -> Let(x, v, simplify_expr e)
    | LetNonDet(x,e) -> LetNonDet(x, simplify_expr e)
    | Call(f,vs) -> Call(f, vs)
    | Choice(e1,e2) -> 
        let e1 = simplify_expr e1 in
        let e2 = simplify_expr e2 in
        if e1 = Skip then e2
        else if e2 = Skip then e1
        else Choice(e1,e2)
    | If(v,e1,e2) -> If(v, simplify_expr e1, simplify_expr e2)
let rec simplify_fundef (x,ys,e) = (x, ys, simplify_expr e)
let rec simplify_prog (fundefs,e) = (List.map simplify_fundef fundefs, simplify_expr e)