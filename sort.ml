open PiSyntax
open SimpleType

let rec sort_t = function
    | SBool -> SBool
    | SInt -> SInt
    | SCh(ts,i) -> 
        let ts' = List.map sort_t ts in
        let intbools,chs = List.partition is_bool_or_int ts' in
        SCh(intbools @ chs, i)

let rec sort_bindings bindings = 
    let bindings' = List.map (fun (y,t) -> (y, sort_t t)) bindings in
    List.partition (fun (_,t) -> is_bool_or_int t) bindings' 

let rec sort_proc bienv chenv = function
    | Nil -> Nil
    | Nu(x,t,p) -> 
        let t' = sort_t t in
        Nu(x, t', sort_proc bienv (M.add x t' chenv) p)
    | In(x,bindings,p) ->
        let (yts,zts) = sort_bindings bindings in
        In(x, yts @ zts, sort_proc (M.add_list yts bienv) (M.add_list zts chenv) p)
    | RIn(x,bindings,p) ->
        let (yts,zts) = sort_bindings bindings in
        RIn(x, yts @ zts, sort_proc (M.add_list yts bienv) (M.add_list zts chenv) p)
    | Out(x,vs,p) -> 
        let (vts1,vts2) = sort_bindings (List.map (fun v -> (v, type_of bienv chenv v)) vs) in
        Out(x, List.map fst (vts1 @ vts2), sort_proc bienv chenv p)
    | Par(p1,p2) -> Par(sort_proc bienv chenv p1, sort_proc bienv chenv p2)
    | If(v,p1,p2) -> If(v, sort_proc bienv chenv p1, sort_proc bienv chenv p2)

let sort p = 
    sort_proc !ext_bienv M.empty p
