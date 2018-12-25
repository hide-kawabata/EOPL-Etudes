(*
  EOPL Compiler for KUE-CHIP2-S
  Copyright (C) 2018 Hideyuki Kawabata
 *)

open Syntax
open Mylist
open Inst

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

(*-- type checking --------------------------------------------------*)
exception NumArgs
exception DevalError
exception WrongNumberOfArguments
exception ApplyError
exception Error of string

let extend_tenv syms types tenv =
  ExtendedTEnvRecord (syms, types, tenv)

let rec apply_tenv tenv sym =
  match tenv with
  | EmptyTEnvRecord -> 
     Printf.fprintf stderr "No type binded for the var.\n"; raise ApplyError
  | ExtendedTEnvRecord (syms, types, old_tenv) ->
     let pos = list_find_position sym syms
     in if pos >= 0 then
	  list_ref types pos
	else
	  apply_tenv old_tenv sym

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
  | Add -> Proctype ([Atomictype "int"; Atomictype "int"], (Atomictype "int"))
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
  | _ -> raise (Error "(Codegen.type_of_primitive)")
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


(*-- code generation --------------------------------------------*)


(* settings *)
let stack_ptr = R 30
let frame_ptr = R 31
(*let stack_ptr_init = 255*)
let stack_ptr_init = 65535
let frame_size = 32
let reg_ret_value = R 29

(* general purpose regs: R 0, ..., R 28 *)
let used_reg = [|false; false; false; false; false; false; false; false;
                 false; false; false; false; false; false; false; false;
                 false; false; false; false; false; false; false; false;
                 false; false; false; false; false|]

let get_reg () =
  let rec iter n = 
    if n < 0 then raise (Error "(get_reg) no reg available")
    else if used_reg.(n) then iter (n-1)
    else let _ = used_reg.(n) <- true in n
  in iter 28

let free_reg n = used_reg.(n) <- false

let func_count = ref 0
let get_func_count () = 
  let v = !func_count in func_count := (v+1); v
                  
let label_count = ref 0
let get_label_count_str () = 
  let v = !label_count in label_count := (v+1); string_of_int v
                  
let empty_env = fun () -> EmptyEnvRecord

let extend_env syms env =
  let len = List.length syms
  in let rec makeseq n lst = 
       if n == 0 then lst else makeseq (n-1) (Val (n)::lst)
     in ExtendedEnvRecord (syms, List.rev (makeseq len []), env)

let extend_env_func syms env =
  let len = List.length syms
  in let rec makeseq n lst = 
       if n == 0 then lst else makeseq (n-1) (Val (n)::lst)
     in FunctionBorder (syms, List.rev (makeseq len []), env)

let extend_env_rec syms env =
 match env with
  | ExtendedEnvRecord (ids, vals, old_env) ->
     let len = List.length syms
     in let rec makeseq n lst = 
          if n == 0 then lst else makeseq (n-1) (Val (n)::lst)
        in (* re-register letrec items *)
        ExtendedEnvRecord (ids, vals, 
              (FunctionBorder (syms, List.rev (makeseq len []), env)))
  | _ -> raise (Error "(extend_env_rec)")


let rec count_offset env = match env with
  | EmptyEnvRecord -> 0
  | FunctionBorder (_, vals, old_env) -> 
     begin match vals with
(*
     | (Val h::t) -> h (* no recursion *)
 *)
     | (Val h::t) -> 0 (* no recursion *)
     | _ -> raise (Error "count_offset")
     end
  | ExtendedEnvRecord (_, vals, old_env) -> 
     begin match vals with
     | (Val h::t) -> h + count_offset old_env
     | _ -> raise (Error "count_offset")
     end
(*
  | RecursivelyExtendedEnvRecord (procs, _, _, old_env) -> 
     let len = List.length procs
     in (len + count_offset old_env)
 *)
  | _ -> raise (Error "(count_offset)")

let rec apply_env env sym = match env with
  | EmptyEnvRecord -> Printf.fprintf stderr "No binding for the var.\n";
                      raise (Error "apply_env")
  | FunctionBorder (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
           let n = match (list_ref vals pos) with
(*
             | Val v -> v
 *)
             | Val v -> -(v+1)
             | _ -> raise (Error "(apply_env)")
           in n (* no recursion *)
	 else
	   apply_env old_env sym
  | ExtendedEnvRecord (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
           let n = match (list_ref vals pos) with
             | Val v -> v
             | _ -> raise (Error "(apply_env)")
           in n + count_offset old_env
	 else
	   apply_env old_env sym
(*
  | RecursivelyExtendedEnvRecord2 (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
           let n = match (list_ref vals pos) with
             | Val v -> v
             | _ -> raise (Error "(apply_env)")
           in n
	 else
	   apply_env old_env sym
  | RecursivelyExtendedEnvRecord (proc_names, idss, bodies, old_env) ->
      let pos = list_find_position sym proc_names
      in if pos >= 0 then
           pos + count_offset old_env
	else
	  apply_env old_env sym
 *)
  | _ -> raise (Error "(apply_env)")


let gen_primitive rator (R dst) =
  let tm = get_reg () in
  let code =
    match rator with
    | Syntax.Add -> 
       [(Pop (R tm), ""); (Pop (R dst), ""); (Add (RR (R dst, R tm)), "prim add")]
    | Syntax.Sub ->
       [(Pop (R tm), ""); (Pop (R dst), ""); (Sub (RR (R dst, R tm)), "prim sub")]
    | Syntax.Mul -> [(Nop, "")]
    | Syntax.Inc -> [(Pop (R dst), ""); 
                     (Add (RImm (R dst, N 1)), "prim inc")]
    | Syntax.Dec -> [(Pop (R dst), ""); 
                     (Sub (RImm (R dst, N 1)), "prim dec")]
    | Syntax.And -> 
       [(Pop (R tm), ""); (Pop (R dst), ""); (And (RR (R dst, R tm)), "prim and")]
    | Syntax.Or ->
       [(Pop (R tm), ""); (Pop (R dst), ""); (Or (RR (R dst, R tm)), "prim or")]
    | Syntax.Not -> [(Pop (R dst), ""); (Not (R dst), "prim not")]
    | Syntax.Gt -> let lab1 = "LA" ^ get_label_count_str ()
                   and lab2 = "LB" ^ get_label_count_str ()
                   in [(Pop (R tm), "");
                       (Pop (R dst), "");
                       (Cmp (RR (R dst, R tm)), "");
                       (Bgt (L lab1), "");
                       (Ld (RImm (R dst, N 0)), "");
                       (Ba (L lab2), "");
                       (Label (L lab1), "");
                       (Ld (RImm (R dst, N 1)), "");
                       (Label (L lab2), "prim gt")]
    | Syntax.Ge -> let lab1 = "LA" ^ get_label_count_str ()
                   and lab2 = "LB" ^ get_label_count_str ()
                   in [(Pop (R tm), "");
                       (Pop (R dst), "");
                       (Cmp (RR (R dst, R tm)), "");
                       (Bge (L lab1), "");
                       (Ld (RImm (R dst, N 0)), "");
                       (Ba (L lab2), "");
                       (Label (L lab1), "");
                       (Ld (RImm (R dst, N 1)), "");
                       (Label (L lab2), "prim ge")]
    | Syntax.Lt -> let lab1 = "LA" ^ get_label_count_str ()
                   and lab2 = "LB" ^ get_label_count_str ()
                   in [(Pop (R tm), "");
                       (Pop (R dst), "");
                       (Cmp (RR (R dst, R tm)), "");
                       (Blt (L lab1), "");
                       (Ld (RImm (R dst, N 0)), "");
                       (Ba (L lab2), "");
                       (Label (L lab1), "");
                       (Ld (RImm (R dst, N 1)), "");
                       (Label (L lab2), "prim lt")]
    | Syntax.Le -> let lab1 = "LA" ^ get_label_count_str ()
                   and lab2 = "LB" ^ get_label_count_str ()
                   in [(Pop (R tm), "");
                       (Pop (R dst), "");
                       (Cmp (RR (R dst, R tm)), "");
                       (Ble (L lab1), "");
                       (Ld (RImm (R dst, N 0)), "");
                       (Ba (L lab2), "");
                       (Label (L lab1), "");
                       (Ld (RImm (R dst, N 1)), "");
                       (Label (L lab2), "prim le")]
    | Syntax.Equal -> let lab1 = "LA" ^ get_label_count_str ()
                      and lab2 = "LB" ^ get_label_count_str ()
                      in [(Pop (R tm), "");
                          (Pop (R dst), "");
                          (Cmp (RR (R dst, R tm)), "");
                          (Bz (L lab1), "");
                          (Ld (RImm (R dst, N 0)), "");
                          (Ba (L lab2), "");
                          (Label (L lab1), "");
                          (Ld (RImm (R dst, N 1)), "");
                          (Label (L lab2), "prim equal")]
    | Syntax.Output -> [(Pop (R tm), "");
                        (Out (R tm), "output to somewhere");
                        (Ld (RImm (R dst, N 0)), "prim output")]
    | _ -> raise (Error "(gen_primitive)")
  in let _ = free_reg tm
     in code

let rec load_locals ids env =
  let ids' = List.rev ids
  in let rec iter ids env code = match ids with
       | [] -> code
       | (h::t) -> let addr = apply_env env h
                   in let tm = get_reg () 
                      in let _ = free_reg tm
                         in iter t env 
                        ([(Pop (R tm), "");
                          (St (RDis (R tm, frame_ptr, L ((string_of_int (-addr))))), 
                           "let " ^ h ^ " = ...")]
                         @ code)
     in iter ids' env []    

(*
let rec load_args ids env =
  let ids' = List.rev ids
  in let rec iter ids env code = match ids with
       | [] -> code
       | (h::t) -> let addr = apply_env env h
                   in let tm = get_reg () 
                      in let _ = free_reg tm
                         in iter t env 
                        ([(Ld (RDis (R tm, frame_ptr, L ((string_of_int (addr+1))))),
                           "load " ^ h ^ " from caller's frame");
                          (St (RDis (R tm, frame_ptr, L ((string_of_int (-addr))))), 
                           "store " ^ h ^ " into callee's frame")]
                         @ code)
     in iter ids' env []    
 *)

let rec gen_iter_pop n (R d) =
  if n = 0 then []
  else (Pop (R d), "cleanup") :: gen_iter_pop (n-1) (R d)

(* list of ASTs --> (code, aux) *)
let rec gen_args args env =
  List.fold_left (fun (l1, l2) arg ->
      let tm = get_reg () 
      in let (code, aux) = gen_expression arg env (R tm)
         in let _ = free_reg tm
            in (l1 @ ([code @ [(Push (R tm), "")]]), l2 @ [aux]))
    ([],[]) args

and gen_args_rec proc_names idss bodies env =
  let rec make_func_names proc_names0 acc = match proc_names0 with
    | [] -> acc
    | (h::t) -> make_func_names t (acc @ [h ^ (string_of_int (get_func_count ()))])
  in let func_names = make_func_names proc_names []
     in
     let rec iter func_names0 idss bodies env (l1, l2) = 
       begin match (func_names0, idss, bodies) with
       | ([], [], []) -> (l1, l2)
       | (func_name::func_names', ids::idss', body::bodies') ->
          let tm = get_reg () in
          let (code, aux) =
(*
         gen_expression (Procexp ([], ids, body)) env (R 22)
 *)
            (* almost the same as the Procexp part *)
            let env' = extend_env_rec ids env
            in let (code0, aux0) = gen_expression body env' (reg_ret_value)
               in let code_aux = 
                    [(Label (L func_name), "");
                     (Comment "Save FP", "");
                     (Push (frame_ptr), "");
                     (Comment "Adjust FP and SP", "");
                     (Ld (RR (frame_ptr, stack_ptr)), "modify FP");
                     (Sub (RImm (stack_ptr, N frame_size)), "adjust SP")]
(*
                     (Sub (RImm (stack_ptr, N frame_size)), "adjust SP");
                     (Comment "Store arguments",
                      "Number of args : " ^ string_of_int (List.length ids))]
                    @ load_args ids env'
 *)                    
                    @ [(Comment "Recursive function names", "")]
                    @ (List.flatten (List.map (fun str -> [(Ld (RImm2 (R tm, L str)), "");
                                                           (Push (R tm), "")])
                                       (List.rev func_names)))
                    @ load_locals proc_names env'
                    @ [(Comment "Function body", "")]
                    @ code0
                    @ [(Comment "Restore FP and SP", "");
                       (Ld (RR (stack_ptr, frame_ptr)), "");
                       (Pop (frame_ptr), ""); (* restore SP *)
                       (Ret, "")]
                  in let code = [(Ld (RImm2 (R tm, L func_name)), "")]
                     in (code, aux0 @ code_aux)
          in 
          let pair = 
            iter func_names' idss' bodies' env (l1 @ [code @ [(Push (R tm), "")]], l2 @ [aux])
          in let _ = free_reg tm
             in pair
       | _ -> raise (Error "(gen_args_rec)")
       end
     in iter func_names idss bodies env ([], [])

and gen_expression exp env (R dst) = (* ast -> env -> reg_t -> (op_t list, op_t list) *)
  match exp with
  | Trueexp -> ([(Ld (RImm (R dst, N 1)), "constant True")], [])
  | Falseexp -> ([(Ld (RImm (R dst, N 0)), "constant False")], [])
  | Litexp i -> ([(Ld (RImm (R dst, N i)), "literal " ^ string_of_int i)], [])
  | Varexp id ->
     let addr = apply_env env id
     in
(*
     if addr < 0 then
          ([(Ld (RImm2 (R dst, L id)), "recursive func")], [])
        else 
 *)
          ([(Ld (RDis (R dst, frame_ptr, L ((string_of_int (-addr))))), "variable " ^ id)], [])
  | Primapp (prim, args) ->
     let (code_for_args, auxs) = gen_args args env (* list of (code * addr) *)
     in let code = (List.flatten code_for_args) @ gen_primitive prim (R dst)
        in (code, List.flatten auxs)
  | Letexp (ids, rands, body) ->
     let (code_for_args, auxs) = gen_args rands env (* (code, aux) *)
     and env' = extend_env ids env
     in let (code0, aux0) = gen_expression body env' (R dst)
        in let code = [(Comment "LET expression", "")] 
                      @ (List.flatten (List.rev code_for_args))
                      @ [(Comment "Store locals", 
                          "Number of locals : " ^ string_of_int (List.length ids))]
                      @ load_locals ids env'
                      @ [(Comment "LET body", "")]
                      @ code0
           in (code, aux0 @ List.flatten auxs)
  | Ifexp (testexp, trueexp, falseexp) ->
     let tm = get_reg ()
     in let (testinst, testaux) = gen_expression testexp env (R tm)
        in let _ = free_reg tm
           in let (trueinst, trueaux) = gen_expression trueexp env (R dst) 
              in let (falseinst, falseaux) = gen_expression falseexp env (R dst) 
                 in let label_f = "LF" ^ get_label_count_str ()
                    and label_e = "LE" ^ get_label_count_str ()
                    in ([(Comment "IF expression", "")]
                        @ testinst
                        @ [(Cmp (RImm ((R tm), N 0)), "");
                           (Bz (L label_f), "")]
                        @ [(Comment "THEN part", "")]
                        @ trueinst 
                        @ [(Ba (L label_e), "")]
                        @ [(Label (L label_f), "")]
                        @ [(Comment "ELSE part", "")]
                        @ falseinst
                        @ [(Label (L label_e), "")],
                        testaux @ trueaux @ falseaux)
  | Procexp (texps, ids, body) ->
     let env' = extend_env_func ids env
     in let (code0, aux0) = gen_expression body env' (reg_ret_value)
        in let func_name = "func"^(string_of_int (get_func_count ()))
           in let code_aux = 
                [(Label (L func_name), "");
                 (Comment "Save FP", "");
                 (Push (frame_ptr), "");
                 (Comment "Adjust FP and SP", "");
                 (Ld (RR (frame_ptr, stack_ptr)), "modify FP");
                 (Sub (RImm (stack_ptr, N frame_size)), "adjust SP")]
(*
                 (Sub (RImm (stack_ptr, N frame_size)), "adjust SP");
                 (Comment "Store arguments",
                  "Number of args : " ^ string_of_int (List.length ids))]
                @ load_args ids env'
 *)
                @ [(Comment "Function body", "")]
                @ code0
                @ [(Comment "Restore FP and SP", "");
                   (Ld (RR (stack_ptr, frame_ptr)), "");
                   (Pop (frame_ptr), ""); (* restore SP *)
                   (Ret, "")]
              in let code = [(Ld (RImm2 (R dst, L func_name)), "")]
                 in (code, aux0 @ code_aux)
  | Appexp (rator, rands) ->
     let tm = get_reg () 
     in let (proc, aux) = gen_expression rator env (R tm)
        and (code_for_args, auxs) = gen_args rands env (* list of code *)
        and iter_pop = gen_iter_pop (List.length rands) (R tm)
        in let code = [(Comment "Evaluate operator", "")]
                      @ proc            
                      @ [(Comment "Prepare arguments", "")]
                      @ List.flatten code_for_args
                      @ [(CallR (R tm), "");
                         (Comment "Obtain returned value", "");
                         (Ld (RR (R dst, reg_ret_value)), "returned value")]
                      @ iter_pop
           in let _ = free_reg tm
           in (code, aux @ List.flatten auxs)
  | Letrecexp (res_texps, proc_names, arg_texpss, idss, bodies, letrec_body) ->
     let env' = extend_env proc_names env (* register proc_names *)
     in let (code_for_args, auxs) = gen_args_rec proc_names idss bodies env'
        in let (code0, aux0) = gen_expression letrec_body env' (R dst)
           in let code = [(Comment "LETREC expression", "")]
                         @ (List.flatten (List.rev code_for_args))
                         @ [(Comment "Store rec funcs",
                             "Number of locals : " ^ string_of_int (List.length proc_names))]
                         @ load_locals proc_names env'
                         @ [(Comment "LETREC body", "")]
                         @ code0
              in (code, aux0 @ List.flatten auxs)

let init_env () = empty_env ()

let arrange_msg str =
  if str = "" then "" else "   \t! " ^ str

let rec print_code = function
  | [] -> Printf.printf "\n"
  | (Nop, msg) :: rest ->
     Printf.printf "\tNOP%s\n" (arrange_msg msg); print_code rest
  | (Out (R r), msg) :: rest -> 
     Printf.printf "\tOUT\tR%d%s\n" r (arrange_msg msg); print_code rest
  | (In (R r), msg) :: rest -> 
     Printf.printf "\tIN\tR%d%s\n" r (arrange_msg msg); print_code rest
  | (Ba (L lab), msg) :: rest ->
     Printf.printf "\tBA\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Bnz (L lab), msg) :: rest ->
     Printf.printf "\tBNZ\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Bz (L lab), msg) :: rest ->
     Printf.printf "\tBZ\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Bn (L lab), msg) :: rest ->
     Printf.printf "\tBN\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Bgt (L lab), msg) :: rest ->
     Printf.printf "\tBGT\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Bge (L lab), msg) :: rest ->
     Printf.printf "\tBGE\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Blt (L lab), msg) :: rest ->
     Printf.printf "\tBLT\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Ble (L lab), msg) :: rest ->
     Printf.printf "\tBLE\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Sra (R r), msg) :: rest ->
     Printf.printf "\tSRA\tR%d%s\n" r (arrange_msg msg); print_code rest
  | (Sla (R r), msg) :: rest ->
     Printf.printf "\tSLA\tR%d%s\n" r (arrange_msg msg); print_code rest
     
  | (Ld (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tLD\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (Ld (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tLD\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (Ld (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tLD\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (Ld (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tLD\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (Ld (RDis (R d, R s, L l)), msg) :: rest ->
     Printf.printf "\tLD\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest
  | (St (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tST\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (St (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tST\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (St (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tST\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (St (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tST\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (St (RDis (R d, R s, L l)), msg) :: rest ->
     Printf.printf "\tST\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest

  | (Add (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tADD\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (Add (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tADD\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (Add (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tADD\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (Add (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tADD\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (Add (RDis (R d, R s, L l)), msg) :: rest ->
     Printf.printf "\tADD\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest
  | (Sub (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tSUB\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (Sub (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tSUB\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (Sub (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tSUB\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (Sub (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tSUB\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (Sub (RDis (R d, R s, L l)), msg) :: rest ->
     Printf.printf "\tSUB\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest
  | (Cmp (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tCMP\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (Cmp (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tCMP\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (Cmp (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tCMP\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (Cmp (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tCMP\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (Cmp (RDis (R d, R s, L l)), msg) :: rest ->
     Printf.printf "\tCMP\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest

  | (And (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tAND\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (And (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tAND\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (And (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tAND\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (And (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tAND\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (And (RDis (R d, R s, L l)), msg) :: rest ->
     Printf.printf "\tAND\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest
  | (Or (RR (R d, R s)), msg) :: rest -> 
     Printf.printf "\tOR\tR%d,\tR%d%s\n" d s (arrange_msg msg); print_code rest
  | (Or (RImm (R d, N n)), msg) :: rest ->
     Printf.printf "\tOR\tR%d,\t%d%s\n" d n (arrange_msg msg); print_code rest
  | (Or (RImm2 (R d, L s)), msg) :: rest ->
     Printf.printf "\tOR\tR%d,\t%s%s\n" d s (arrange_msg msg); print_code rest
  | (Or (RDir (R d, L l)), msg) :: rest -> 
     Printf.printf "\tOR\tR%d,\t[%s]%s\n" d l (arrange_msg msg); print_code rest
  | (Or (RDis (R d, R s, L l)),msg) :: rest ->
     Printf.printf "\tOR\tR%d,\t[R%d+%s]%s\n" d s l (arrange_msg msg); print_code rest
  | (Not (R d), msg) :: rest -> 
     Printf.printf "\tNOT\tR%d%s\n" d (arrange_msg msg); print_code rest

  | (CallR (R d), msg) :: rest ->
     Printf.printf "\tCALLR\tR%d%s\n" d (arrange_msg msg); print_code rest
  | (CallI (L lab), msg) :: rest -> 
     Printf.printf "\tCALLI\t%s%s\n" lab (arrange_msg msg); print_code rest
  | (Ret, msg) :: rest -> 
     Printf.printf "\tRET%s\n" (arrange_msg msg); print_code rest
  | (Label (L lab), msg) :: rest -> 
     Printf.printf "%s:%s\n" lab (arrange_msg msg); print_code rest
  | (End, msg) :: rest -> 
     Printf.printf "\tEND%s\n" (arrange_msg msg); print_code rest
  | (Push (R d), msg) :: rest -> 
     Printf.printf "\tPUSH\tR%d%s\n" d (arrange_msg msg); print_code rest
  | (Pop (R d), msg) :: rest -> 
     Printf.printf "\tPOP\tR%d%s\n" d (arrange_msg msg); print_code rest
  | (Comment s, msg) :: rest -> 
     Printf.printf "* %s%s\n" s (arrange_msg msg); print_code rest
  | (Hlt, msg) :: rest -> 
     Printf.printf "\tHLT%s\n" (arrange_msg msg); print_code rest

  | _ -> Printf.printf "Too long.\n"


let gen_program = function
    Program body -> let (code, aux) = gen_expression body (init_env ()) (reg_ret_value)
                    in let code' = [(Label (L "start"), "");
                                    (Comment "Initialize Frame Pointer", "");
                                    (Ld (RImm (frame_ptr, N stack_ptr_init)), "");
                                    (Comment "Initialize Stack Pointer", "");
                                    (Ld (RR (stack_ptr, frame_ptr)), "");
                                    (Sub (RImm (stack_ptr, N frame_size)), "");
                                    (Label (L "main"), "")]
                                   @ code
                                   @ [(Hlt, "")]
                                   @ aux
                                   @ [(End, "")]
                       in print_code code'; 0

let gen_from_string str =
  gen_program (Eopl_parser.program Eopl_lexer.token (Lexing.from_string str))
    

let prog = "let f = proc (int x, int y) let a = 3 b = 4 f = proc (int x, int y) -(x,y) in +(+(x,y),(f a b)) in (f 5 4)";;

let ast = (Eopl_parser.program Eopl_lexer.token (Lexing.from_string prog));;
