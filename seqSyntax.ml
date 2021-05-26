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

(** syntax of sequential programs **)
type prog = fundef list * expr


(** pretty-print **)
open Format
open Utilities
open PiSyntax

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

let pp_print_fundef ppf (f,ys,e) =
    fprintf ppf "@[int %s%a {@  %a@ }@]" f (pp_print_list pp_print_string) ys pp_print_expr e

let pp_print_mainfundef ppf e =
    fprintf ppf "@[int main() {@ %a@ }@]" pp_print_expr e 

let pp_print_prog ppf (fundefs,e) =
    fprintf ppf "@[#define true 1\n#define false 0\nint nondet() { int n; return n; }@ %a@ %a@]" (pp_print_list ~left:"" ~right:"" ~delimiter:"" pp_print_fundef) fundefs pp_print_mainfundef e

let print_prog oc prog = fprintf (formatter_of_out_channel oc) "%a@." pp_print_prog prog


let rec subst_expr map = function
    | Skip -> Skip
    | Let(x,v,e) -> Let(x, subst_val map v, subst_expr (M.remove x map) e)
    | LetNonDet(x,e) -> LetNonDet(x, subst_expr (M.remove x map) e)
    | Call(f,vs) -> Call(f, List.map (subst_val map) vs)
    | Choice(e1,e2) -> Choice(subst_expr map e1, subst_expr map e2)
    | If(v,e1,e2) -> If(subst_val map v, subst_expr map e1, subst_expr map e2)
    | Assume(v,e) -> Assume(subst_val map v, subst_expr map e)
