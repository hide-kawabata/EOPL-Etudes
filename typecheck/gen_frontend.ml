let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
	try
	  let parsed = Eopl_parser.program Eopl_lexer.token lexbuf
	  in let _ = Codegen.type_of_program parsed
	  in let _ = Codegen.gen_program parsed
(*
	  in let resultval = Codegen.gen_program parsed
             in Printf.fprintf stderr "Return code = %d.\n" resultval; flush stdout
 *)
             in flush stdout
	with
	    Parsing.Parse_error -> Printf.printf "Syntax error.\n";
  	  | Codegen.TypeMismatch -> Printf.printf "Type error.\n";
	  | Codegen.NumArgs -> Printf.printf "Number of arguments mismatched.\n";
	  | Codegen.ApplyError -> Printf.printf "Apply error.\n";
  with Eopl_lexer.Eof ->
    Printf.printf "\n"; exit 0
