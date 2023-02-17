open PiSyntax

type region' = R of int | RVar of region' option ref
type t' = TBool | TInt | TCh of t' list * region' | TVar of t' option ref

exception UnifyT of t' * t'

let extenv = ref M.empty 

let rec unify_region r1 r2 =
    match r1, r2 with
    | R(i), R(j) when i = j -> ()
    | RVar(ref1), RVar(ref2) when ref1 == ref2 -> ()
    | RVar({ contents = Some(r1') }), _ -> unify_region r1' r2
    | _, RVar({ contents = Some(r2') }) -> unify_region r1 r2'
    | RVar({ contents = None } as ref1), _ -> ref1 := Some(r2)
    | _, RVar({ contents = None } as ref2) -> ref2 := Some(r1)
    | _ -> assert false

let rec occur ref = function
    | TBool | TInt -> false
    | TCh(ts,_) -> List.exists (occur ref) ts
    | TVar(ref2) when ref == ref2 -> true
    | TVar({ contents = None }) -> false
    | TVar({ contents = Some(t) }) -> occur ref t

let rec unify_t t1 t2 =
    match t1, t2 with
    | TBool, TBool -> ()
    | TInt, TInt -> ()
    | TCh(ts1,r1), TCh(ts2,r2) -> 
        if List.length ts1 <> List.length ts2 then raise (UnifyT(t1,t2));
        List.iter2 unify_t ts1 ts2;
        unify_region r1 r2
    | TVar(ref1), TVar(ref2) when ref1 == ref2 -> ()
    | TVar({ contents = Some(t1') }), _ -> unify_t t1' t2
    | _, TVar({ contents = Some(t2') }) -> unify_t t1 t2'
    | TVar({ contents = None } as ref1), _ -> 
        if occur ref1 t2 then raise (UnifyT(t1,t2));
        ref1 := Some(t2)
    | _, TVar({ contents = None } as ref2) -> 
        if occur ref2 t1 then raise (UnifyT(t1,t2));
        ref2 := Some(t1)
    | _ -> raise (UnifyT(t1,t2))

let op_table =
    [(NOT  , ([TBool       ], TBool));
         (AND  , ([TBool; TBool], TBool));
         (OR   , ([TBool; TBool], TBool));
         (EQ   , ([TInt ; TInt ], TBool));
         (LT   , ([TInt ; TInt ], TBool));
         (GT   , ([TInt ; TInt ], TBool));
         (LE   , ([TInt ; TInt ], TBool));
         (GE   , ([TInt ; TInt ], TBool));
         (MINUS, ([TInt        ], TInt ));
         (ADD  , ([TInt ; TInt ], TInt ));
         (SUB  , ([TInt ; TInt ], TInt ));
         (MUL  , ([TInt ; TInt ], TInt ));
         (DIV  , ([TInt ; TInt ], TInt ))]
let rec infer_val env = function
    | Var(x) when M.mem x env -> M.find x env
    | Var(x) when M.mem x !extenv -> M.find x !extenv
    | Var(x) -> 
        (* print_string x; *)
        let t = TVar(ref None) in
        extenv := M.add x t !extenv;
        t
    | Bool(_) -> TBool
    | Int(_) -> TInt
    | Op(op,vs) ->
        let ts, t = List.assoc op op_table in
        List.iter2 (fun v t -> unify_t (infer_val env v) t) vs ts;
        t      
    
let rec infer_proc env = function
    | Nil -> Nil
    | Nu(x,_,p) -> 
        let t' = TVar(ref None) in
        Nu(x, t', infer_proc (M.add x t' env) p)
    | In(x,yts,p) -> 
        let yts' = List.map (fun (y,_) -> (y, TVar(ref None))) yts in
        unify_t (infer_val env (Var(x))) (TCh(List.map snd yts', RVar(ref None)));
        In(x, yts', infer_proc (M.add_list yts' env) p)
    | RIn(x,yts,p) -> 
        let yts' = List.map (fun (y,_) -> (y, TVar(ref None))) yts in
        unify_t (infer_val env (Var(x))) (TCh(List.map snd yts', RVar(ref None)));
        RIn(x, yts', infer_proc (M.add_list yts' env) p)
    | Out(x,vs,p) ->
        unify_t (infer_val env (Var(x))) (TCh(List.map (infer_val env) vs, RVar(ref None)));
        Out(x, vs, infer_proc env p)
    | Par(p1,p2) -> Par(infer_proc env p1, infer_proc env p2)
    | If(v,p1,p2) ->
        unify_t (infer_val env v) TBool;
        If(v, infer_proc env p1, infer_proc env p2)


let region_num = ref 0

let rec deref_region = function
    | R(i) -> R(i)
    | RVar({ contents = Some(r) } as ref) ->
        let r' = deref_region r in
        ref := Some(r');
        r'
    | RVar({ contents = None } as ref) ->
        let r = R(incr region_num; !region_num) in
        (* Printf.printf "make region %d.\n" !region_num; *)
        ref := Some(r);
        r

let rec deref_t = function
    | TBool -> TBool
    | TInt -> TInt
    | TCh(ts,r) -> TCh(List.map deref_t ts, deref_region r) 
    | TVar({ contents = Some(t) } as ref) ->
        let t' = deref_t t in
        ref := Some(t');
        t'
    | TVar({ contents = None } as ref) ->
        (* print_string "Ch() ni shita"; *)
        let t = TCh([], R(incr region_num; !region_num)) in
        ref := Some(t);
        t

let rec extract_t = function
    | TBool -> SimpleType.SBool
    | TInt -> SimpleType.SInt
    | TCh(ts,R(i)) -> SimpleType.SCh(List.map extract_t ts, i)
    | _ -> assert false

let rec deref_proc = function
    | Nil -> Nil
    | Nu(x,t,p) -> 
        let t' = extract_t (deref_t t) in
        (match t' with SimpleType.SCh(_) -> () | _ -> failwith "new int");  (* x : chan-type *)
        Nu(x, t', deref_proc p)
    | In(x,yts,p) -> In(x, List.map (fun (y,t) -> (y, extract_t (deref_t t))) yts, deref_proc p)
    | RIn(x,yts,p) -> RIn(x, List.map (fun (y,t) -> (y, extract_t (deref_t t))) yts, deref_proc p)
    | Out(x,vs,p) -> Out(x, vs, deref_proc p)
    | Par(p1,p2) -> Par(deref_proc p1, deref_proc p2)
    | If(v,p1,p2) -> If(v, deref_proc p1, deref_proc p2)


let close extenv p = 
    let bienv,chenv = M.partition (fun _ t -> SimpleType.is_bool_or_int t) extenv in
    SimpleType.ext_bienv := bienv;
    M.fold_right (fun x t p -> Nu(x, t, p)) chenv p


let typing p = 
    extenv := M.empty;
    (* SimpleType.extenv := M.empty; *)
    SimpleType.ext_bienv := M.empty;
    region_num := 0;
    let p' = infer_proc M.empty p in
    close (M.map (fun t -> extract_t (deref_t t)) !extenv) (deref_proc p')
