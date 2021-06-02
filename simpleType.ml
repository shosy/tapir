(** syntax of simple types **)
type t =
    | SBool
    | SInt
    | SCh of t list * int    (* the int parameter denotes the region of the channel *)

(** external type environment **)
let ext_bienv = ref M.empty 

(** types of operators **)
(* open PiSyntax
let op_table =
    [(NOT  , ([SBool       ], SBool));
     (AND  , ([SBool; SBool], SBool));
     (OR   , ([SBool; SBool], SBool));
     (EQ   , ([SInt ; SInt ], SBool));
     (LT   , ([SInt ; SInt ], SBool));
     (GT   , ([SInt ; SInt ], SBool));
     (LE   , ([SInt ; SInt ], SBool));
     (GE   , ([SInt ; SInt ], SBool));
     (MINUS, ([SInt        ], SInt ));
     (ADD  , ([SInt ; SInt ], SInt ));
     (SUB  , ([SInt ; SInt ], SInt ));
     (MUL  , ([SInt ; SInt ], SInt ));
     (DIV  , ([SInt ; SInt ], SInt ))] *)

(* open Printf
let rec print_t oc = function
    | SBool -> fprintf oc "bool"
    | SInt -> fprintf oc "int"
    | SCh(ts,i) -> fprintf oc "ch" *)
    (* | SCh(ts,i) -> Utilities.print_list ~left:"ch(" ~right:("; "^string_of_int i^")") ~delimiter:", " print_t outchan ts *)

open Format
open Utilities
let rec pp_print_t ppf = function
    | SBool -> pp_print_string ppf "bool" 
    | SInt -> pp_print_string ppf "int"
    | SCh(ts,i) -> 
        fprintf ppf "ch(@[%a;@ region=%d@])" (pp_print_list pp_print_t) ts i   (* 改善の余地*)


let is_bool t = (t = SBool)
let is_int t = (t = SInt)
let is_bool_or_int t = is_bool t || is_int t
let is_ch = function SCh(_) -> true | _ -> false


open PiSyntax
let type_of bienv chenv = function
    | Var(x) -> 
        (try M.find x bienv with Not_found ->
         try M.find x chenv with Not_found ->
         assert false)
    | Bool(_) -> SBool
    | Int(_) -> SInt
    | Op(NOT,_) | Op(AND,_) | Op(OR,_) | Op(IMPLY,_) | Op(EQ,_) | Op(LT,_) | Op(GT,_) | Op(LE,_) | Op(GE,_) -> SBool
    | Op(MINUS,_) | Op(ADD,_) | Op(SUB,_) | Op(MUL,_) | Op(DIV,_) -> SInt
    | Unknown(_) -> SBool
    | Exists(_) -> SBool
