(** syntax of simple types **)
type t =
    | SBool
    | SInt
    | SCh of t list * int    (* the int parameter denotes the region of the channel *)

(** external type environment **)
let extenv = ref M.empty

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
