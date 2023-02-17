open PiSyntax

let extmap = ref M.empty

let new_var =
    let num = ref 0 in
    fun x -> 
        incr num;
        x ^ "_" ^ string_of_int !num

let alpha_var map x = 
    try M.find x map with Not_found ->
    try M.find x !extmap with Not_found ->
    let x' = new_var x in
    extmap := M.add x x' !extmap;
    x'

let rec alpha_val map = function
    | Var(x) -> Var(alpha_var map x)
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | Op(op,vs) -> Op(op, List.map (alpha_val map) vs)

let rec alpha_proc map = function
    | Nil -> Nil
    | Nu(x,t,p) -> 
        let x' = new_var x in
        Nu(x', t, alpha_proc (M.add x x' map) p)
    | In(x,yts,p) ->
        let x' = alpha_var map x in
        let yts' = List.map (fun (y,t) -> (new_var y, t)) yts in
        In(x', yts', alpha_proc (M.add_list (List.map2 (fun (y,_) (y',_) -> (y,y')) yts yts') map) p)
    | RIn(x,yts,p) ->
        let x' = alpha_var map x in
        let yts' = List.map (fun (y,t) -> (new_var y, t)) yts in
        RIn(x', yts', alpha_proc (M.add_list (List.map2 (fun (y,_) (y',_) -> (y,y')) yts yts') map) p)
    | Out(x,vs,p) -> Out(alpha_var map x, List.map (alpha_val map) vs, alpha_proc map p)
    | Par(p1,p2) -> Par(alpha_proc map p1, alpha_proc map p2)
    | If(v,p1,p2) -> If(alpha_val map v, alpha_proc map p1, alpha_proc map p2)

let alphaconv p = 
    alpha_proc M.empty p
