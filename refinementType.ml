type t =
    (* | RBool  *)
    | RInt of string * PiSyntax.value
    | RCh of (string * t) list * int


type rTypeEnvEl = Bind of string * t | Val of PiSyntax.value 
type rTypeEnv = rTypeEnvEl list

let extenv = ref []


open Format 
open Utilities
open PiSyntax

let rec pp_print_t ppf = function
    | RInt(x,v) -> fprintf ppf "@[{%s : int | %a}@]" x pp_print_val v
    | RCh(zts,i) -> 
        fprintf ppf "ch(@[%a;@ region=%d@])" (pp_print_list (pp_print_pair pp_print_string pp_print_t)) zts i




let rec subst_t map = function
    | RInt(x,v) -> RInt(x, subst_val (M.remove x map) v)
    | RCh(zts,i) -> RCh(List.map (fun (z,t) -> (z, subst_t (M.remove_list (List.map fst zts) map) t)) zts, i)  (* 改善?? *)
        (* remove 不要?? *)