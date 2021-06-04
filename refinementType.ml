type t =
    | RBool
    | RInt
    | RCh of (string * t) list * PiSyntax.value * t list * PiSyntax.value * t list * int


(* type rTypeEnvEl = Bind of string * t | Val of PiSyntax.value  *)
(* type rTypeEnv = rTypeEnvEl list *)

let ext_bienv = ref M.empty


open Format 
open Utilities
open PiSyntax

let rec pp_print_t ppf = function
    | RBool -> pp_print_string ppf "bool"
    | RInt -> pp_print_string ppf "int"
    | RCh(yts,vI,tsI,vO,tsO,i) -> 
        fprintf ppf "ch(@[%a;@ %a;@ %a;@ %a;@ %a;@ region=%d@])" 
            (pp_print_list (pp_print_pair pp_print_string pp_print_t)) yts 
            pp_print_val vI 
            (pp_print_list pp_print_t) tsI 
            pp_print_val vO
            (pp_print_list pp_print_t) tsO 
            i




let rec subst_t map = function
    | RBool -> RBool
    | RInt -> RInt
    | RCh(yts,vI,tsI,vO,tsO,i) ->
        let sigma = M.remove_list (List.map fst yts) map in 
        RCh(yts, subst_val sigma vI, List.map (subst_t sigma) tsI, 
                 subst_val sigma vO, List.map (subst_t sigma) tsO, i)
        (* remove 不要?? *)



let type_of bienv chenv = function
    | Var(x) -> (try M.find x bienv with Not_found -> M.find x chenv)
    | Bool(_) | Unknown(_) | Exists(_) -> RBool
    | Int(_) -> RInt
    | Op(op,_) -> 
        (match op with 
        | NOT | AND | OR | IMPLY | EQ | LT | GT | LE | GE -> RBool
        | MINUS | ADD | SUB | MUL | DIV -> RInt)


let is_bool = function RBool -> true | _ -> false
let is_int = function RInt -> true | _ -> false
let is_bool_or_int t = is_bool t || is_int t
let is_ch = function RCh(_) -> true | _ -> false