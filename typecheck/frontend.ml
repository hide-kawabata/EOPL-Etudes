let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	Printf.printf "eopl> "; flush stdout;
	try
	  let parsed = Eopl_parser.program Eopl_lexer.token lexbuf
	  in let _ = Eopl.type_of_program parsed
	  in let resultval = Eopl.eval_program parsed
	  in match resultval with
	      Syntax.Val result -> Printf.printf "%d: int\n" result; flush stdout
	    | Syntax.BoolVal result ->
		Printf.printf "%s: bool\n" (if result then "true" else "false"); flush stdout
	    | _ -> Printf.printf "Result not an integer.\n"; flush stdout
	with
	    Parsing.Parse_error -> Printf.printf "Syntax error.\n";
  	  | Eopl.TypeMismatch -> Printf.printf "Type error.\n";
	  | Eopl.NumArgs -> Printf.printf "Number of arguments mismatched.\n";
	  | Eopl.ApplyError -> Printf.printf "Apply error.\n";
      done
  with Eopl_lexer.Eof ->
    Printf.printf "\n"; exit 0
