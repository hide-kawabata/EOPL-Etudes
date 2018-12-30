let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
        try
          let parsed = Eopl_parser2.program Eopl_lexer2.token lexbuf
          in let ast = Codegen2.type_of_program parsed
          in let _ = Codegen2.gen_program ast
(*
          in let _ = Codegen2.type_of_program parsed
          in let _ = Codegen2.gen_program parsed
 *)
(*
          in let resultval = Codegen.gen_program parsed
             in Printf.fprintf stderr "Return code = %d.\n" resultval; flush stdout
 *)
             in flush stdout
        with
            Parsing.Parse_error -> Printf.printf "Syntax error.\n";
          | Codegen2.TypeMismatch -> Printf.printf "Type error.\n";
          | Codegen2.NumArgs -> Printf.printf "Number of arguments mismatched.\n";
          | Codegen2.ApplyError -> Printf.printf "Apply error.\n";
  with Eopl_lexer2.Eof ->
    Printf.printf "\n"; exit 0
