let simple_mode = ref false
let z3_mode = ref false

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
    let logchan = open_out (filename^".log") in
    (* Format.pp_print_string (Format.formatter_of_out_channel logchan) "****** alpha conversion ******\n";  *)
    output_string logchan "****** alpha conversion ******\n"; 
    PiSyntax.print_proc logchan alpha_proc;
    let simpletyped_proc = SimpleTyping.typing alpha_proc in
    (* Format.pp_print_string (Format.formatter_of_out_channel logchan) "****** simple type ******\n";  *)
    output_string logchan "\n****** simple type ******\n"; 
    PiSyntax.print_proc ~pp_print_t:SimpleType.pp_print_t logchan simpletyped_proc;
    (* let sorted_proc = Sort.sort simpletyped_proc in
    output_string logchan "\n****** sorted ******\n"; 
    PiSyntax.print_proc ~pp_print_t:SimpleType.pp_print_t logchan sorted_proc; *)
    (* if !simple_mode then ( *)
        let simpletransformed_prog = SimpleTransform.transform simpletyped_proc in
        (* let simpletransformed_prog = SimpleTransform.transform sorted_proc in *)
        (* SeqSyntax.print_prog stdout simpletransformed_prog; *)
        let mlchan = open_out (filename^".simple.ml") in
        SeqSyntax.print_prog mlchan simpletransformed_prog;
        close_out mlchan
        (* let _ = Sys.command ("cd UAutomizer-linux/; ./Ultimate.py --spec ../PropertyTermination.prp --file "^("../"^filename^".simple.c")^" --architecture 64bit > "^("../"^filename^".simple.result")^"; cd ../") in
        (* close_out outchan *)
        let ic = open_in (filename^".simple.result") in
        let rec f () = 
            try 
                let line = input_line ic in
                if line = "TRUE" then print_string "TERMINATE!\n"
                else f ()
            with End_of_file -> print_string "UNKNOWN\n"
        in f();
        close_in ic *)
    (* ) else (
        let (refinementtyped_proc, chc) = RefinementTyping.typing sorted_proc in
        (* Format.pp_print_string (Format.formatter_of_out_channel logchan) "****** refinement type ******\n";  *)
        output_string logchan "\n****** refinement type ******\n"; 
        PiSyntax.print_proc ~pp_print_t:RefinementType.pp_print_t logchan refinementtyped_proc;
        let refinementtransformed_prog = RefinementTransform.transform refinementtyped_proc in
        (* Format.pp_print_string (Format.formatter_of_out_channel logchan) "****** C program ******\n";  *)
        output_string logchan "\n****** C program ******\n"; 
        SeqSyntax.print_prog logchan refinementtransformed_prog;
        close_out logchan;
        let smt2chan = open_out (filename^".smt2") in
        RefinementTyping.print_smt2 smt2chan chc;
        close_out smt2chan;
        let model = 
        if not !z3_mode then (
        let _ = Sys.command ("hoice "^(filename^".smt2")^" > "^(filename^".hoice")) in
        let hoicechan = open_in (filename^".hoice") in
        let lexbuf = Lexing.from_channel hoicechan in
        let parsed_hoice_model = ModelParser.toplevel ModelLexer.token lexbuf in
        let hoicemodel = ModelSyntax.del_unknown (ModelSyntax.del_exists parsed_hoice_model) in
        close_in hoicechan;
        hoicemodel
        ) else (
        let _ = Sys.command ("hoice "^(filename^".smt2")^" > "^(filename^".hoice")) in
        let hoicechan = open_in (filename^".hoice") in
        let lexbuf = Lexing.from_channel hoicechan in
        let parsed_hoice_model = ModelParser.toplevel ModelLexer.token lexbuf in
        let hoicemodel = ModelSyntax.del_unknown (ModelSyntax.del_exists parsed_hoice_model) in
        close_in hoicechan;
        let _ = Sys.command ("z3 "^(filename^".smt2")^" > "^(filename^".z3")) in
        let z3chan = open_in (filename^".z3") in
        let lexbuf = Lexing.from_channel z3chan in
        let parsed_z3_model = Z3Parser.toplevel Z3Lexer.token lexbuf in
        let z3model = ModelSyntax.del_unknown (ModelSyntax.del_exists parsed_z3_model) in
        close_in z3chan;
        ModelSyntax.merge hoicemodel z3model
        ) in
        let completed = ModelSyntax.apply_prog model refinementtransformed_prog in
        let cchan = open_out (filename^".c") in
        SeqSyntax.print_prog cchan completed;
        close_out cchan;
        let _ = Sys.command ("cd UAutomizer-linux/; ./Ultimate.py --spec ../PropertyTermination.prp --file "^("../"^filename^".c")^" --architecture 64bit > "^("../"^filename^".result")^"; cd ../") in
        let chc = ref chc in
        let ic = open_in (filename^".result") in
        let rec f () = 
            try 
                let line = input_line ic in
                if line = "TRUE" then print_string "TERMINATE!\n"
                else f ()
            with End_of_file -> 
                (
                try
                    let flag = ref true in
                    let addchc = AddCHC.main refinementtransformed_prog in
                    PiSyntax.print_val stdout addchc;
                    (* RefinementTyping.print_smt2 stdout [addchc]; *)
                    chc := !chc @ [addchc];
                    let smt2chan = open_out (filename^".smt2") in
                    RefinementTyping.print_smt2 smt2chan !chc;
                    close_out smt2chan;
                    let model = 
                        if not !z3_mode then (
                        let _ = Sys.command ("hoice "^(filename^".smt2")^" > "^(filename^".hoice")) in
                        let hoicechan = open_in (filename^".hoice") in
                        (try
                        let lexbuf = Lexing.from_channel hoicechan in
                        let parsed_hoice_model = ModelParser.toplevel ModelLexer.token lexbuf in
                        let hoicemodel = ModelSyntax.del_unknown (ModelSyntax.del_exists parsed_hoice_model) in
                        close_in hoicechan;
                        hoicemodel
                        with _ -> (* unsat, unknown *)
                            let rec del_last = function
                                | [] -> []
                                | [x] -> []
                                | x::xs -> x::del_last xs
                            in
                            print_string "unsat or unknown\n";
                            chc := del_last !chc;
                            flag := false;
                            []
                        )
                        ) else (
                        let _ = Sys.command ("hoice "^(filename^".smt2")^" > "^(filename^".hoice")) in
                        let hoicechan = open_in (filename^".hoice") in
                        let lexbuf = Lexing.from_channel hoicechan in
                        let parsed_hoice_model = ModelParser.toplevel ModelLexer.token lexbuf in
                        let hoicemodel = ModelSyntax.del_unknown (ModelSyntax.del_exists parsed_hoice_model) in
                        close_in hoicechan;
                        let _ = Sys.command ("z3 "^(filename^".smt2")^" > "^(filename^".z3")) in
                        let z3chan = open_in (filename^".z3") in
                        let lexbuf = Lexing.from_channel z3chan in
                        let parsed_z3_model = Z3Parser.toplevel Z3Lexer.token lexbuf in
                        let z3model = ModelSyntax.del_unknown (ModelSyntax.del_exists parsed_z3_model) in
                        close_in z3chan;
                        ModelSyntax.merge hoicemodel z3model
                        ) in
                    if !flag then (
                    let completed = ModelSyntax.apply_prog model refinementtransformed_prog in
                    let cchan = open_out (filename^".c") in
                    SeqSyntax.print_prog cchan completed;
                    close_out cchan;
                    let _ = Sys.command ("cd UAutomizer-linux/; ./Ultimate.py --spec ../PropertyTermination.prp --file "^("../"^filename^".c")^" --architecture 64bit > "^("../"^filename^".result")^"; cd ../") in
                    f () 
                    )
                    else
                    f ()
                with _ ->  
                    print_string "UNKNOWN\n"
                )
        in f();
        close_in ic
    ) *)



let main () =
    let filenames = ref [] in
    Arg.parse 
        [("-s", Arg.Set(simple_mode), "simple type-based transformation");
         ("-z3", Arg.Set(z3_mode), "Z3")] 
        (fun f -> filenames := !filenames @ [f]) 
        "Hello World!";
    let filename = 
        match !filenames with 
        | [f] -> f 
        | [] -> failwith "input one file"
        | _ -> failwith "input only one file"
    in 
    (* let start_t = Sys.time () in *)
    file filename
    (* let end_t = Sys.time () in *)
    (* print_float (end_t -. start_t); *)
    (* print_newline () *)

let _ =
    main ()
