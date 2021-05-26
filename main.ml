let simple_mode = ref false

let file filename =
    (* if not (Filename.check_suffix filename ".pi") then failwith "kakuchoushi NOT .pi"; *)
    (* let f = Filename.chop_suffix filename ".pi" in *)
    let inchan = open_in filename in    (* if error then ...... *)
    let lexbuf = Lexing.from_channel inchan in
    let parsed_proc = PiParser.toplevel PiLexer.token lexbuf in
    close_in inchan;
    let alpha_proc = Alphaconv.alphaconv parsed_proc in
    (* let outchan = open_out (filename^".tmp") in *)
    (* PiSyntax.print_proc outchan alpha_proc; *)
    PiSyntax.print_proc stdout alpha_proc;
    let simpletyped_proc = SimpleTyping.typing alpha_proc in
    PiSyntax.print_proc ~pp_print_t:SimpleType.pp_print_t stdout simpletyped_proc;
    if !simple_mode then (
        let simpletransformed_prog = SimpleTransform.transform simpletyped_proc in
        SeqSyntax.print_prog stdout simpletransformed_prog;
        (* close_out outchan *)
        ()
    ) else (
        let (refinementtyped_proc, chc) = RefinementTyping.typing simpletyped_proc in
        PiSyntax.print_proc ~pp_print_t:RefinementType.pp_print_t stdout refinementtyped_proc;
        let refinementtransformed_prog = RefinementTransform.transform refinementtyped_proc in
        SeqSyntax.print_prog stdout refinementtransformed_prog;
        let smt2chan = open_out (filename^".smt2") in
        RefinementTyping.print_smt2 smt2chan chc;
        close_out smt2chan;
        let _ = Sys.command ("hoice "^(filename^".smt2")^" > "^(filename^".hoice")) in
        let hoicechan = open_in (filename^".hoice") in
        let lexbuf = Lexing.from_channel hoicechan in
        let parsed_model = ModelParser.toplevel ModelLexer.token lexbuf in
        close_in hoicechan;
        let completed = ModelSyntax.apply_prog parsed_model refinementtransformed_prog in
        SeqSyntax.print_prog stdout completed;
        ()
    )



let main () =
    let filenames = ref [] in
    Arg.parse 
        [("-s", Arg.Set(simple_mode), "simple type-based transformation")] 
        (fun f -> filenames := !filenames @ [f]) 
        "Hello World!";
    let filename = 
        match !filenames with 
        | [f] -> f 
        | [] -> failwith "input one file"
        | _ -> failwith "input only one file"
    in 
    let start_t = Sys.time () in
    file filename;
    let end_t = Sys.time () in
    print_float (end_t -. start_t)

let _ =
    main ()
