open Syntax
open Mylist

let rec list_index pred ls =
  if ls = [] then -1
  else if pred (car ls) then 0
  else let list_index_r = list_index pred (cdr ls)
       in if list_index_r >= 0 then list_index_r + 1 else -1

let list_find_position sym los =
  list_index (fun sym1 -> sym1 = sym) los

let rec list_ref ls pos =
  if pos = 0 then (car ls)
  else list_ref (cdr ls) (pos-1)


(* samples *)
let p1 = Program (Primapp (Add, [(Litexp 8); (Litexp 7)]))
let p2 = Program (Primapp (Add, [(Varexp "x"); (Litexp 7)]))
let p3 = Program (Letexp (["x"], [(Litexp 100)],
		  Primapp (Add, [(Varexp "x"); (Litexp 7)])))

let empty_env = fun () -> EmptyEnvRecord

let extend_env = fun syms vals env -> ExtendedEnvRecord (syms, vals, env)

let extend_env_recursively =
  fun proc_names idss bodies old_env ->
    RecursivelyExtendedEnvRecord (proc_names, idss, bodies, old_env)

exception WrongNumberOfArguments
exception ApplyError
exception Error of string

let rec apply_env = fun env sym -> match env with
    EmptyEnvRecord -> Printf.fprintf stderr "No binding for the var.\n"; raise ApplyError
  | ExtendedEnvRecord (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
	  list_ref vals pos
	else
	  apply_env old_env sym
  | RecursivelyExtendedEnvRecord (proc_names, idss, bodies, old_env) ->
      let pos = list_find_position sym proc_names
      in if pos >= 0 then
	  Closure (list_ref idss pos, list_ref bodies pos, env)
	else
	  apply_env old_env sym
  | _ -> raise (Error "(Eopl.apply_env)")

let extend_tenv syms types tenv =
  ExtendedTEnvRecord (syms, types, tenv)

let rec apply_tenv tenv sym =
  match tenv with
      EmptyTEnvRecord -> Printf.fprintf stderr "No type binded for the var.\n"; raise ApplyError
    | ExtendedTEnvRecord (syms, types, old_tenv) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
	  list_ref types pos
	else
	  apply_tenv old_tenv sym


exception NumArgs
exception DevalError
let get_int vals =
  List.map
    (fun x -> match x with
	 Val i -> i
       | _ -> raise DevalError)
    vals
let get_bool vals =
  List.map
    (fun x -> match x with
	 BoolVal i -> i
       | _ -> raise DevalError)
    vals

let apply_primitive rator args =
  match rator with
      Add -> Val (foldl (+) (get_int args))
    | Sub -> Val (foldl (-) (get_int args))
    | Mul -> Val (foldl ( * ) (get_int args))
    | Inc -> Val ((List.hd (get_int args)) + 1)
    | Dec -> Val ((List.hd (get_int args)) - 1)
    | And -> BoolVal (foldl ( && ) (get_bool args))
    | Or -> BoolVal (foldl ( || ) (get_bool args))
    | Not -> BoolVal (not (List.hd (get_bool args)))
    | Gt -> let lst = get_int args in let x = car lst and y = cadr lst in BoolVal (x > y)
    | Ge -> let lst = get_int args in let x = car lst and y = cadr lst in BoolVal (x >= y)
    | Lt -> let lst = get_int args in let x = car lst and y = cadr lst in BoolVal (x < y)
    | Le -> let lst = get_int args in let x = car lst and y = cadr lst in BoolVal (x <= y)
    | Equal -> let lst = get_int args in let x = car lst and y = cadr lst in BoolVal (x == y)
    | Output -> Val (car (get_int args))
    | _ -> raise (Error ("Eopl.apply_primitive"))
	
let eval_bool_exp = function
    BoolVal x -> if x then true else false
  | _ -> raise ApplyError


let rec eval_args args env =
  List.map (fun x -> eval_expression x env) args
and eval_expression exp env =
  match exp with
      Trueexp -> BoolVal true
    | Falseexp -> BoolVal false
    | Litexp i -> Val i
    | Varexp id -> apply_env env id
    | Primapp (prim, args) -> 
	let args_evaluated = eval_args args env
	in apply_primitive prim args_evaluated
    | Letexp (ids, rands, body) ->
	let args_evaluated = eval_args rands env
	in eval_expression body (extend_env ids args_evaluated env)
    | Ifexp (testexp, trueexp, falseexp) ->
	if eval_bool_exp (eval_expression testexp env) then
	  eval_expression trueexp env
	else
	  eval_expression falseexp env
    | Procexp (texps, ids, body) -> Closure (ids, body, env)
    | Appexp (rator, rands) ->
	let proc = eval_expression rator env
	and args = eval_args rands env
	in
	  (match proc with
	       Closure(_, _, _) -> apply_procval proc args
	     | _ -> raise ApplyError)
    | Letrecexp (res_texps, proc_names, arg_texpss, idss, bodies, letrec_body) ->
	eval_expression letrec_body (extend_env_recursively proc_names idss bodies env)

and apply_procval proc args =
  match proc with
      Closure (ids, body, env) ->
	if not (List.length ids == List.length args) then
	  raise WrongNumberOfArguments
	else
	  eval_expression body (extend_env ids args env)
    | _ -> raise ApplyError

let init_env () =
(*
  extend_env ["i"; "v"; "x"; "emptylist"]
    [Val 1; Val 5; Val 10; Val (-1)]
 *)
    (empty_env ())
    
let eval_program = function
    Program body -> eval_expression body (init_env ())
  
let eval_from_string str =
  eval_program (Eopl_parser.program Eopl_lexer.token (Lexing.from_string str))


exception TypeMismatch

let rec check_equal_type_iter4 idss arg_typess bodies result_types tenv_for_body =
  if List.length idss == 0 then ()
  else begin
    check_equal_type
      (type_of_expression
	 (List.hd bodies)
	 (extend_tenv (List.hd idss) (List.hd arg_typess) tenv_for_body))
      (List.hd result_types);
    check_equal_type_iter4
      (List.tl idss) (List.tl arg_typess) (List.tl bodies) (List.tl result_types)
      tenv_for_body
  end
and check_equal_types tys1 tys2 =
  if List.length tys1 == 0 then ()
  else begin
    check_equal_type (List.hd tys1) (List.hd tys2);
    check_equal_types (List.tl tys1) (List.tl tys2)
  end
and check_equal_type t1 t2 =
    match t1 with
	Atomictype id1 -> begin
	  match t2 with
	      Atomictype id2 ->
		if compare id1 id2 == 0 then ()
		else raise TypeMismatch
	    | _ -> raise TypeMismatch
	end
      | Proctype (tys1, ty1) -> begin
	  match t2 with
	      Proctype (tys2, ty2) ->
		if not (List.length tys1 == List.length tys2) then raise TypeMismatch
		else begin
		  check_equal_types tys1 tys2;
		  check_equal_type ty1 ty2
		end
	    | _ -> raise TypeMismatch
	end
and type_of_expression exp tenv =
  match exp with
      Litexp n -> Atomictype "int"
    | Trueexp -> Atomictype "bool"
    | Falseexp -> Atomictype "bool"
    | Varexp i -> apply_tenv tenv i
    | Ifexp (test_exp, true_exp, false_exp) ->
	let test_type = type_of_expression test_exp tenv
	and false_type = type_of_expression false_exp tenv
	and true_type = type_of_expression true_exp tenv
	in begin
	    check_equal_type test_type (Atomictype "bool");
	    check_equal_type true_type false_type;
	    true_type
	  end
    | Procexp (texps, ids, body) ->
	type_of_proc_exp texps ids body tenv
    | Primapp (prim, rands) ->
	type_of_application (type_of_primitive prim) (types_of_expressions rands tenv)
    | Appexp (rator, rands) ->
	type_of_application (type_of_expression rator tenv) (types_of_expressions rands tenv)
    | Letexp (ids, rands, body) ->
	type_of_let_exp ids rands body tenv
    | Letrecexp (result_texps, proc_names, texpss, idss, bodies, letrec_body) ->
	type_of_letrec_exp 
	  result_texps proc_names texpss idss bodies letrec_body tenv
and types_of_expressions rands tenv =
  List.map (fun exp -> type_of_expression exp tenv) rands
and type_of_proc_exp texps ids body tenv =
  let arg_types = texps
  in let result_type = (type_of_expression body (extend_tenv ids arg_types tenv))
  in Proctype (arg_types, result_type)
and type_of_application rator_type rand_types =
  match rator_type with
      Proctype (arg_types, result_type) ->
	if List.length arg_types == List.length rand_types then begin
	  check_equal_types rand_types arg_types;
	  result_type
	end
	else raise TypeMismatch
    | _ -> raise TypeMismatch
and type_of_primitive = function
    Add -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "int"))
  | Sub -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "int"))
  | Mul -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "int"))
  | Inc -> Proctype ([Atomictype "int"], (Atomictype "int"))
  | Dec -> Proctype ([Atomictype "int"], (Atomictype "int"))
  | And -> Proctype ([Atomictype "bool"; Atomictype "bool"], (Atomictype "bool"))
  | Or -> Proctype ([Atomictype "bool"; Atomictype "bool"], (Atomictype "bool"))
  | Not -> Proctype ([Atomictype "bool"], (Atomictype "bool"))
  | Gt -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "bool"))
  | Ge -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "bool"))
  | Lt -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "bool"))
  | Le -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "bool"))
  | Equal -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "bool"))
  | Output -> Proctype ([Atomictype "int"], (Atomictype "bool"))
  | _ -> raise (Error "(Eopl.type_of_primitive)")
and type_of_let_exp ids rands body tenv =
  let tenv_for_body = extend_tenv ids (types_of_expressions rands tenv) tenv
  in type_of_expression body tenv_for_body
and type_of_letrec_exp result_texps proc_names texpss idss bodies letrec_body tenv =
  let arg_typess = texpss
  and result_types = result_texps
  in let the_proc_types = List.map2 (fun a r -> Proctype (a, r)) arg_typess result_types
  in let tenv_for_body = extend_tenv proc_names the_proc_types tenv
  in begin
      check_equal_type_iter4 idss arg_typess bodies result_types tenv_for_body;
      type_of_expression letrec_body tenv_for_body
    end


let init_tenv () =
  extend_tenv [] [] EmptyTEnvRecord

let type_of_program = function
    Program body -> type_of_expression body (init_tenv ())
