open PiSyntax

let rec normalize = function
  | Nil -> Nil
  | Nu(x,t,p) -> 
      let p = normalize p in
      if S.mem x (fv_proc p) then Nu(x,t,p) else p
  | In(x,yts,p) -> In(x, yts, normalize p)
  | RIn(x,yts,p) -> RIn(x, yts, normalize p)
  | Out(x,vs,p) -> Out(x, vs, normalize p)
  | Par(ps) -> 
      let ps = List.map normalize ps in
      let ps = List.filter ((<>) Nil) ps in
      (match ps with
      | [] -> Nil
      | [p] -> p
      | _ -> 
          let ret = List.fold_left 
            (fun ls p -> (match p with Nil -> ls | Par(ps') -> ls @ ps' | _ -> ls @ [p]))
            []
            ps
          in Par(ret))
  | If(v,p1,p2) -> If(v, normalize p1, normalize p2)
