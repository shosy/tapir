open Format

let pp_print_pair pp_print_t1 pp_print_t2 ppf (x1,x2) =
    fprintf ppf "@[%a : %a@]" pp_print_t1 x1 pp_print_t2 x2

let rec pr_list ~delimiter pp_print_t ppf = function
    | [] -> ()
    | [x] -> pp_print_t ppf x 
    | x::xs -> fprintf ppf "%a%s@ %a" pp_print_t x delimiter (pr_list ~delimiter:delimiter pp_print_t) xs

let pp_print_list ?(left="(") ?(right=")") ?(delimiter=",") pp_print_t ppf ls = 
    fprintf ppf "%s@[<hv 0>%a@]%s" left (pr_list ~delimiter:delimiter pp_print_t) ls right
