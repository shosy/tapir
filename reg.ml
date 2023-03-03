open SimpleType
open PiSyntax

type reg = int
let regof = function
    | SCh(_,i) -> i
    | _ -> assert false


let tmp = ref 0
let newreg () = 
    incr tmp;
    !tmp

let rec newty = function
    | SBool -> SBool
    | SInt -> SInt
    | SCh(ts,_) -> 
        let i = newreg () in
        SCh(List.map newty ts, i)

let rec newproc tyenv = function
    | Nil -> Nil
    | Nu(x,t,p) -> 
        let t' = newty t in
        Nu(x, t', newproc (M.add x t tyenv) p)
    | In(x,yts,p1,p2) -> 
        let yts' = List.map (fun (y,t) -> (y, newty t)) yts in
        In(x, yts', newproc (M.add_list yts' tyenv) p1, newproc (M.add_list yts' tyenv) p2)
    | RIn(x,yts,p) -> 
        let yts' = List.map (fun (y,t) -> (y, newty t)) yts in
        RIn(x, yts', newproc (M.add_list yts' tyenv) p)
    | Out(x,vs,p) -> Out(x, vs, newproc tyenv p)
    | Par(ps) -> Par(List.map (newproc tyenv) ps)
    | If(v,p1,p2) -> If(v, newproc tyenv p1, newproc tyenv p2)




let notstar = ref S.empty

exception Noreg of reg
exception Eqreg of reg * reg

let rec inf tyenv = function
    | Nil -> Nil
    | Nu(x,t,p) -> 
        let reg = regof t in
        if S.mem (regof t) !notstar then
            Nu(x, t, inf (M.add x t tyenv) p)             
        else
            raise (Noreg reg)
    | In(x,yts,p1,p2) -> 
        let t = type_of M.empty tyenv (Var(x)) in
        let reg = regof t in
        if S.mem (regof t) !notstar then
            In(x, yts, inf (M.add_list yts tyenv) p1, inf (M.add_list yts tyenv) p2)
        else
            raise (Noreg reg)
    | RIn(x,yts,p) -> 
        let t = type_of M.empty tyenv (Var(x)) in
        let reg = regof t in
        if S.mem (regof t) !notstar then
            RIn(x, yts, inf (M.add_list yts tyenv) p)
        else
            raise (Noreg reg)
    | Out(x,vs,p) -> 
        (match type_of M.empty tyenv (Var(x)) with SCh(ts',_) ->
            List.iter2 (fun t' v -> 
                if is_ch t' && S.mem (regof t') !notstar then 
                    if regof t' <> regof (type_of M.empty tyenv v) then
                        raise (Eqreg(regof t', regof (type_of M.empty tyenv v)))
            ) ts' vs
        | _ -> assert false);
        Out(x, vs, inf tyenv p)
    | Par(ps) -> Par(List.map (inf tyenv) ps)
    | If(v,p1,p2) -> If(v, inf tyenv p1, inf tyenv p2)


let rec substregty reg1 reg2 = function
    | SBool -> SBool
    | SInt -> SInt
    | SCh(ts,i) -> 
        if i = reg1 then
            SCh(List.map (substregty reg1 reg2) ts, reg2)
        else
            SCh(List.map (substregty reg1 reg2) ts, i)
let rec substreg reg1 reg2 = function
    | Nil -> Nil
    | Nu(x,t,p) -> Nu(x, substregty reg1 reg2 t, substreg reg1 reg2 p)
    | In(x,yts,p1,p2) -> In(x, List.map (fun (y,t) -> (y, substregty reg1 reg2 t)) yts, substreg reg1 reg2 p1, substreg reg1 reg2 p2)
    | RIn(x,yts,p) -> RIn(x, List.map (fun (y,t) -> (y, substregty reg1 reg2 t)) yts, substreg reg1 reg2 p)
    | Out(x,vs,p) -> Out(x, vs, substreg reg1 reg2 p)
    | Par(ps) -> Par(List.map (substreg reg1 reg2) ps)
    | If(v,p1,p2) -> If(v, substreg reg1 reg2 p1, substreg reg1 reg2 p2)


let rec inferb proc =
    try
        inf M.empty proc
    with
        | Noreg(reg) -> 
            notstar := S.add reg !notstar;
            inferb proc
        | Eqreg(reg1,reg2) ->
            let proc' = substreg reg1 reg2 proc in
            inferb proc'




let rec starty = function
    | SCh(ts,i) -> 
        if S.mem i !notstar then SCh(List.map starty ts, i)
        else SCh(List.map starty ts, -1)  (* -1はstarを表す *)
    | t -> t
let rec starp = function
    | Nil -> Nil
    | Nu(x,t,p) -> Nu(x, starty t, starp p)
    | In(x,yts,p1,p2) -> In(x, List.map (fun (y,t) -> (y, starty t)) yts, starp p1, starp p2)
    | RIn(x,yts,p) -> RIn(x, List.map (fun (y,t) -> (y, starty t)) yts, starp p)
    | Out(x,vs,p) -> Out(x, vs, starp p)
    | Par(ps) -> Par(List.map starp ps)
    | If(v,p1,p2) -> If(v, starp p1, starp p2)

let infer proc = 
    notstar := S.empty;
    starp (inferb (newproc M.empty proc))
