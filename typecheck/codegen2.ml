(*
  EOPL Compiler for KUE-CHIP2-S
  Copyright (C) 2018 Hideyuki Kawabata
 *)

open Syntax2
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


let makeseq m = 
  let rec iter n lst =
    if n = 0 then lst else iter (n-1) (m-n::lst)
  in iter m []
   


(*-- type checking --------------------------------------------------*)
exception NumArgs
exception DevalError
exception WrongNumberOfArguments
exception ApplyError
exception Error of string

let extend_tenv syms types tenv =
  ExtendedTEnvRecord (syms, types, tenv)

let extend_tenv_func syms types tenv =
  ExtendedTEnvRecord_F (syms, types, tenv)

let extend_tenv_rec syms types tenv =
  ExtendedTEnvRecord_R (syms, types, tenv)

let rec apply_tenv tenv sym = (* (type, names) *)
  match tenv with
  | EmptyTEnvRecord -> 
     Printf.fprintf stderr "No type binded for the var.\n"; raise ApplyError
  | ExtendedTEnvRecord (syms, types, old_tenv) ->
     let pos = list_find_position sym syms
     in if pos >= 0 then
          list_ref types pos
        else
          apply_tenv old_tenv sym
  | ExtendedTEnvRecord_F (syms, types, old_tenv) ->
     let pos = list_find_position sym syms
     in if pos >= 0 then
          list_ref types pos
        else
          let (ty, _) = apply_tenv old_tenv sym
          in (ty, [sym])
  | ExtendedTEnvRecord_R (syms, types, old_tenv) ->
     let pos = list_find_position sym syms
     in if pos >= 0 then
          let (ty, _) = list_ref types pos
          in (ty, [sym])
        else
          let (ty, _) = apply_tenv old_tenv sym
          in (ty, [sym])

exception TypeMismatch

let uniq_names = List.sort_uniq (fun s t -> if s < t then -1 else if s > t then 1 else 0)

let rec split3 lst = match lst with
  | [] -> ([], [], [])
  | (x,y,z)::rest -> let (xs, ys, zs) = split3 rest in (x::xs, y::ys, z::zs)
        
(* (globalss, bodies_ast)  *)
let rec check_equal_type_iter4 idss arg_typess bodies result_types tenv_for_body =
  if List.length idss == 0 then ([], [])
  else begin
      let (ty, names, body) = 
        (type_of_expression
           (List.hd bodies)
           (extend_tenv (List.hd idss)
              (List.map (fun ty -> (ty, [])) (List.hd arg_typess)) tenv_for_body))
      in begin
      check_equal_type ty (List.hd result_types);
      let (globalss', bodies_ast') = 
        (check_equal_type_iter4
           (List.tl idss) (List.tl arg_typess) (List.tl bodies) (List.tl result_types)
           tenv_for_body)
      in
      (names :: globalss', body :: bodies_ast')
        end
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
and type_of_expression exp tenv = (* env -> tenv -> (type * names) *)
  match exp with
      Litexp n -> (Atomictype "int", [], Litexp_A n)
    | Trueexp -> (Atomictype "bool", [], Trueexp_A)
    | Falseexp -> (Atomictype "bool", [], Falseexp_A)
    | Varexp i -> let (ty, syms) = apply_tenv tenv i in (ty, syms, Varexp_A i)
    | Ifexp (test_exp, true_exp, false_exp) ->
       let (test_type, test_names, test_tr) = type_of_expression test_exp tenv
       and (false_type, false_names, false_tr) = type_of_expression false_exp tenv
       and (true_type, true_names, true_tr) = type_of_expression true_exp tenv
       in begin
           check_equal_type test_type (Atomictype "bool");
           check_equal_type true_type false_type;
           (true_type, uniq_names (test_names @ false_names @ true_names),
            Ifexp_A (test_tr, true_tr, false_tr))
         end
    | Procexp (texps, ids, body) ->
       let arg_types = texps
       in let arg_types' = List.map (fun ty -> (ty, [])) arg_types
          in let (result_type, names, body_ast) =
               type_of_expression body (extend_tenv_func ids arg_types' tenv)
             in (Proctype (arg_types, result_type), [],
                 Procexp_A (texps, ids, body_ast, names))

    | Primapp (prim, rands) ->
       let (tys, namess, asts) = split3 (types_of_expressions rands tenv)
       in let tys' = type_of_application (type_of_primitive prim) tys (* [ty] *)
          in (tys', uniq_names (List.flatten namess), Primapp_A (prim, asts))

    | Appexp (rator, rands) ->
       let (rands_tys, rands_namess, rands_asts) =
         split3 (types_of_expressions rands tenv) (* [ty] *)
       in let (rator_tys, rator_namess, rator_ast) = 
            (type_of_expression rator tenv) (* [ty] *)
          in let tys' = type_of_application rator_tys rands_tys
             in (tys', uniq_names (List.flatten (rands_namess) @ rator_namess),
                 Appexp_A (rator_ast, rands_asts))

    | Letexp (ids, rands, body) ->
       let tyns = types_of_expressions rands tenv (* [(type, [name])] *)
       in let (ty0s, namess, asts) = split3 tyns
          in let tenv_for_body = extend_tenv ids (List.combine ty0s namess) tenv
             in let (ty_body, names_body, body_ast) = type_of_expression body tenv_for_body
                in (ty_body, uniq_names ((List.flatten namess) @ names_body),
                    Letexp_A (ids, asts, body_ast))

    | Letrecexp (result_texps, proc_names, texpss, idss, bodies, letrec_body) ->
       let arg_typess = texpss
       and result_types = result_texps
       in let the_proc_types =
            List.map2 (fun a r -> Proctype (a, r)) arg_typess result_types
          in let the_proc_types' = List.map (fun ty -> (ty, [])) the_proc_types
          in let tenv_for_body = extend_tenv_rec proc_names the_proc_types' tenv
          in begin
                 let (globalss, bodies_ast) = 
                   check_equal_type_iter4 idss arg_typess bodies result_types tenv_for_body
                 in let (ty, names, letrec_body_ast) = 
                      type_of_expression letrec_body tenv_for_body
                    in (ty, [],
                        Letrecexp_A (result_texps, proc_names, texpss, idss, bodies_ast,
                                     letrec_body_ast, globalss))
               end

and types_of_expressions rands tenv = (* [(type, [name])] *)
  List.map (fun exp -> type_of_expression exp tenv) rands

and type_of_application rator_type rand_types = (* ty -> ty -> ty *)
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


let init_tenv () =
  extend_tenv [] [] EmptyTEnvRecord

let type_of_program = function
    Program body -> let (_, _, ast) = type_of_expression body (init_tenv ()) in Program_A ast


(*-- code generation --------------------------------------------*)


(* settings *)
let stack_ptr = R 30
let frame_ptr = R 31
let heap_ptr_init = 255
let stack_ptr_init = 65535
(* let frame_size = 32 *)
let reg_ret_value = R 29
let heap_ptr = R 28

(* general purpose regs: R 0, ..., R 27 *)
let used_reg = [|false; false; false; false; false; false; false; false;
                 false; false; false; false; false; false; false; false;
                 false; false; false; false; false; false; false; false;
                 false; false; false; false|]

let get_reg () =
  let rec iter n = 
    if n < 0 then raise (Error "(get_reg) no reg available")
    else if used_reg.(n) then iter (n-1)
    else let _ = used_reg.(n) <- true in n
  in iter 27

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
  in ExtendedEnvRecord (syms, List.map (fun n -> Val (n + 1))  (makeseq len), env)

let extend_env_func syms env =
  let len = List.length syms
  in FunctionBorder (syms, List.map (fun n -> Val (n + 1)) (makeseq len), env)

let extend_env_globals syms env =
  let len = List.length syms
  in ExtendedEnvRecord_G (syms, List.map (fun n -> Val (n + 1)) (makeseq len), env)

let rec count_offset env = match env with
  | EmptyEnvRecord -> 0
  | FunctionBorder (_, vals, old_env) -> 
     begin match vals with
     | (Val h::t) -> 0 (* no recursion *)
     | _ -> raise (Error "count_offset")
     end
  | ExtendedEnvRecord (_, vals, old_env) -> 
     begin match vals with
     | (Val h::t) -> h + count_offset old_env
     | _ -> raise (Error "count_offset")
     end
  | _ -> raise (Error "(count_offset)")

let rec apply_env env sym = match env with
  | EmptyEnvRecord -> Printf.fprintf stderr "No binding for the var.\n";
                      raise (Error "apply_env")
  | FunctionBorder (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
           let n = match (list_ref vals pos) with
             | Val v -> -(v+2)
             | _ -> raise (Error "(apply_env)1")
           in n (* no recursion *)
         else
           apply_env old_env sym
  | ExtendedEnvRecord (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
           let n = match (list_ref vals pos) with
             | Val v -> v
             | _ -> raise (Error "(apply_env)2")
           in n + count_offset old_env
         else
           apply_env old_env sym
  | ExtendedEnvRecord_G (syms, vals, old_env) ->
      let pos = list_find_position sym syms
      in if pos >= 0 then
           let n = match (list_ref vals pos) with
             | Val v -> v
             | _ -> raise (Error "(apply_env)22")
           in n + 1000
         else
           apply_env old_env sym
  | _ -> raise (Error "(apply_env)4")

let gen_primitive rator (R dst) =
  let tm = get_reg () in
  let code =
    match rator with
    | Syntax2.Add -> 
       [(Pop (R tm), ""); (Pop (R dst), ""); (Add (RR (R dst, R tm)), "prim add")]
    | Syntax2.Sub ->
       [(Pop (R tm), ""); (Pop (R dst), ""); (Sub (RR (R dst, R tm)), "prim sub")]
    | Syntax2.Mul -> [(Nop, "")]
    | Syntax2.Inc -> [(Pop (R dst), ""); 
                     (Add (RImm (R dst, N 1)), "prim inc")]
    | Syntax2.Dec -> [(Pop (R dst), ""); 
                     (Sub (RImm (R dst, N 1)), "prim dec")]
    | Syntax2.And -> 
       [(Pop (R tm), ""); (Pop (R dst), ""); (And (RR (R dst, R tm)), "prim and")]
    | Syntax2.Or ->
       [(Pop (R tm), ""); (Pop (R dst), ""); (Or (RR (R dst, R tm)), "prim or")]
    | Syntax2.Not -> [(Pop (R dst), ""); (Not (R dst), "prim not")]
    | Syntax2.Gt -> let lab1 = "LA" ^ get_label_count_str ()
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
    | Syntax2.Ge -> let lab1 = "LA" ^ get_label_count_str ()
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
    | Syntax2.Lt -> let lab1 = "LA" ^ get_label_count_str ()
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
    | Syntax2.Le -> let lab1 = "LA" ^ get_label_count_str ()
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
    | Syntax2.Equal -> let lab1 = "LA" ^ get_label_count_str ()
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
    | Syntax2.Output -> [(Pop (R tm), "");
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

let gen_call_seq n (R d) =
  let rec gen_iter_pop n (R d) =
    if n = 0 then []
    else (Pop (R d), "cleanup") :: gen_iter_pop (n-1) (R d)
  in (CallR (R d), "") :: gen_iter_pop n (R d)

let gen_prelude func_name n_locals =
  [(Label (L func_name), "");
   (Comment "Save FP", "");
   (Push (frame_ptr), "");
   (Comment "Adjust FP and SP", "");
   (Ld (RR (frame_ptr, stack_ptr)), "modify FP");
   (Sub (RImm (stack_ptr, N n_locals)), "adjust SP")]

let gen_postlude = 
  [(Comment "Restore FP and SP", "");
   (Ld (RR (stack_ptr, frame_ptr)), "");
   (Pop (frame_ptr), ""); (* restore SP *)
   (Ret, "")]

let rec gen_args args env = (* (code, aux, nlocals) *)
  let (l1, l2, n) = 
  List.fold_left (fun (l1, l2, n) arg ->
      let tm = get_reg () 
      in let (code, aux, nlocals) = gen_expression arg env (R tm) "_norecname_"
         in let _ = free_reg tm
            in (l1 @ ([code @ [(Push (R tm), "")]]), l2 @ [aux], max n nlocals))
    ([], [], 0) args
  in (l1, l2, n)

and procexp_gen_pair func_name ids body globals (R dst) env recname =
  let env' = extend_env_func ids (extend_env_globals globals env)
  in let (code0, code_aux0, fs) = gen_expression body env' (reg_ret_value) recname
     in let code_aux =
          code_aux0 
          @ gen_prelude func_name fs
          @ [(Comment "Global variables", "")]
          @ (List.map (fun name -> (Comment name, "global")) globals)
          @ [(Comment "Function body", "")]
          @ [(Label (L (recname ^ "_entry")), "")]
          @ code0
          @ gen_postlude
      in let code = (* generate a closure *)
          let r0 = get_reg ()
          in let c0 = [(Comment "Proc", "");
                       (Ld (RImm2 (R r0, L "heap_alloc")), "")]
          in let r1 = get_reg ()
          in let c1 = [(Ld (RImm (R r1, N (List.length globals + 2))), "size");
                       (Push (R r1), "")]
          in let c2 = gen_call_seq 1 (R r0)
                      @ [(Ld (RImm2 (R r0, L func_name)), "");
                         (St (RDis (R r0, reg_ret_value, L "0")), "store func ptr")]
          in let r2 = get_reg ()
          in let c3 = 
               if List.length globals <= 0 then (* no global references *)
                 [(Ld (RImm (R r0, N 0)), "null");
                  (St (RDis (R r0, reg_ret_value, L "1")), "store env ptr")]
               else (* global references *)
                 [(Ld (RR (R r2, reg_ret_value)), "");
                  (Add (RImm (R r2, N 2)), "");
                  (St (RDis (R r2, reg_ret_value, L "1")), "store env ptr")]
                 @ List.flatten
                     (List.map2
                        (fun name n -> (* oldenv *)
                          let (c', _, _) = 
                            gen_expression (Varexp_A name) env (R r1) "_norecname_"
                          in let c'' = 
                               c' @ [(St (RDis (R r1, R r2, L (string_of_int n))), 
                                      "env entry")]
                             in c'') 
                        globals 
                        (makeseq (List.length globals)))
          in let _ = free_reg r1
          in let _ = free_reg r2
          in let c4 = [(Ld (RR (R dst, reg_ret_value)), "closure ptr")]
          in let _ = free_reg r0
          in c0 @ c1 @ c2 @ c3 @ c4
        in (code, code_aux, 0)

and gen_args_rec proc_names idss bodies env globalss =
  let rec make_func_names proc_names0 acc = match proc_names0 with
    | [] -> acc
    | (h::t) -> make_func_names t (acc @ [h ^ (string_of_int (get_func_count ()))])
  in let func_names = make_func_names proc_names []
     in
     let rec iter func_names0 proc_names0 idss bodies env globalss (l1, l2) = 
       begin match (func_names0, proc_names0, idss, bodies, globalss) with
       | ([], [], [], [], []) -> (l1, l2)
       | (func_name::func_names', proc_name::proc_names',
          ids::idss', body::bodies', globals::globalss') ->
          let tm = get_reg () in
          let (code, aux, _) = (* almost the same as the Procexp part *)
            procexp_gen_pair func_name ids body globals (R tm) env proc_name
          in 
          let pair = 
            iter func_names' proc_names' idss' bodies' env globalss'
              (l1 @ [code @ [(Push (R tm), "")]], l2 @ [aux])
          in let _ = free_reg tm
             in pair
       | _ -> raise (Error "(gen_args_rec)")
       end
     in iter func_names proc_names idss bodies env globalss ([], [])

and gen_expression exp env (R dst) recname = (* (op_t list, op_t list, int) *)
  match exp with
  | Trueexp_A -> ([(Ld (RImm (R dst, N 1)), "constant True")], [], 0)
  | Falseexp_A -> ([(Ld (RImm (R dst, N 0)), "constant False")], [], 0)
  | Litexp_A i -> ([(Ld (RImm (R dst, N i)), "literal " ^ string_of_int i)], [], 0)
  | Varexp_A id ->
     let addr = apply_env env id
     in
     if addr > 1000 then (* global variables *)
       ([(Ld (RDis (R dst, frame_ptr, L((string_of_int (2))))), "env ptr");
         (Ld (RDis (R dst, R dst, L((string_of_int (addr-1000-1))))), "global " ^ id)],
        [], 0)
     else 
       ([(Ld (RDis (R dst, frame_ptr, L ((string_of_int (-addr))))), "variable " ^ id)], 
        [], 0)
  | Primapp_A (prim, args) ->
     let (code_for_args, auxs, _) = gen_args args env (* list of (code * addr) *)
     in let code = (List.flatten code_for_args) @ gen_primitive prim (R dst)
        in (code, List.flatten auxs, 0)
  | Letexp_A (ids, rands, body) ->
     let (code_for_args, auxs, n_locals_args) = gen_args rands env (* (code, aux) *)
     and env' = extend_env ids env
     in let name' = if List.exists (fun s -> s = recname) ids
                    then "_norecname_" else recname
     in let (code0, aux0, n_locals_body0) = gen_expression body env' (R dst) name'
     in let n_locals_vars0 = List.length ids
     in let n_locals_body = n_locals_body0 + n_locals_vars0
        in let code = [(Comment "LET expression", "")] 
                      @ (List.flatten (List.rev code_for_args))
                      @ [(Comment "Store locals", 
                          "Number of locals : " ^ string_of_int n_locals_vars0)]
                      @ load_locals ids env'
                      @ [(Comment "LET body", "")]
                      @ code0
           in (code, aux0 @ List.flatten auxs, max n_locals_body n_locals_args)
  | Ifexp_A (testexp, trueexp, falseexp) ->
     let tm = get_reg ()
     in let (testinst, testaux, _) = gen_expression testexp env (R tm) "_norecname_"
        in let _ = free_reg tm
           in let (trueinst, trueaux, a) = gen_expression trueexp env (R dst) recname
              in let (falseinst, falseaux, b) = gen_expression falseexp env (R dst) recname
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
                        testaux @ trueaux @ falseaux, max a b)
  | Procexp_A (texps, ids, body, globals) ->
     let func_name = "func" ^ (string_of_int (get_func_count ()))
     in procexp_gen_pair func_name ids body globals (R dst) env recname

  | Appexp_A (rator, rands) ->

        (* If this call is a tailcall, 
           - store values of operands into arguments
           - use BA instead of CALLR
         *)
                 
     let (code0, flag) =
       let tailcall_enable = true
       in match rator with
          | Varexp_A id -> if tailcall_enable && id = recname then
                             ([(Comment ("tailcall: " ^ id), recname)], true)
                           else 
                             ([(Comment ("no tailcall: " ^ id), recname)], false)
          | _ -> ([], false)

     in let tm = get_reg () 
     in let (proc, aux, a) = gen_expression rator env (R tm) recname
     in let c_op = if flag then
                     [] (* no need for evaluating the operand *)
                   else
                     ((Comment "Evaluate operator", "") :: proc)
     in let r2 = get_reg ()
     in let c_env = if flag then
                      [(Comment "Modify arguments", "")] (* no need for updating envptr *)
                    else
                      [(Ld (RDis (R r2, R tm, L "1")), "envptr");
                       (Comment "Prepare arguments", "")]
     in let (c_args, auxs, b) = gen_args rands env (* list of code for args *)
     in let c_param = if flag then (* update parameters *)
                        List.flatten 
                          (List.map (fun n -> 
                               [(Pop (R r2), "");
                                (St (RDis (R r2, frame_ptr, L (string_of_int (n+3)))),
                                 "param")])
                             (List.rev (makeseq (List.length c_args)))
                          )
                      else (* pass envptr as an argument *)
                        [(Push (R r2), "envptr as arg")]
     in let c_addr = [(Ld (RDis (R tm, R tm, L "0")), "addr")] (* callee's address *)
     in let _ = free_reg r2
     in let c_jmp = if flag then
                      [(Ba (L (recname ^ "_entry")), "tail call")]
                    else
                      gen_call_seq ((List.length rands) + 1) (R tm)
                      @ [(Comment "Obtain returned value", "");
                         (Ld (RR (R dst, reg_ret_value)), "returned value")]
     in let _ = free_reg tm
        in (code0 @ c_op @ c_env @ List.flatten c_args @ c_param @ c_addr @ c_jmp,
            aux @ List.flatten auxs, max a b)

  | Letrecexp_A (res_texps, proc_names, arg_texpss, idss, bodies, letrec_body, globalss) ->
     let env' = extend_env proc_names env (* register proc_names *)
     in let (code_for_args, auxs) = gen_args_rec proc_names idss bodies env' globalss
     in let (code0, aux0, n_locals_body0) = gen_expression letrec_body env' (R dst) recname
     in let code = [(Comment "LETREC expression", "")]
                   @ (List.flatten (List.rev code_for_args))
                   @ [(Comment "Store rec funcs",
                       "Number of locals : " ^ string_of_int (List.length proc_names))]
                   @ load_locals proc_names env'
                   @ [(Comment "LETREC body", "")]
                   @ code0
        in (code, aux0 @ List.flatten auxs, n_locals_body0 + List.length proc_names)


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


let gen_program (Program_A body) =
  let (code, aux, fs) = gen_expression body (init_env ()) (reg_ret_value) "_norecname_"
  in let code' = [(Label (L "start"), "");
                  (Comment "Initialize Heap Pointer", "");
                  (Ld (RImm (heap_ptr, N heap_ptr_init)), "");
                  (Comment "Initialize Frame Pointer", "");
                  (Ld (RImm (frame_ptr, N stack_ptr_init)), "");
                  (Comment "Initialize Stack Pointer", "");
                  (Ld (RR (stack_ptr, frame_ptr)), "");
                  (Sub (RImm (stack_ptr, N fs)), "adjust SP");
                  (Label (L "main"), "")]
                 @ code
                 @ [(Hlt, "")]
                 @ aux
                 @ gen_prelude "heap_alloc" 0
                 @ [(Ld (RR (reg_ret_value, heap_ptr)), "returned ptr");
                    (Add (RDis (heap_ptr, frame_ptr, L "2")), "size")]
                 @ gen_postlude
                 @ [(End, "")]
     in print_code code'; 0

let gen_from_string str =
  let parsed =  (Eopl_parser2.program Eopl_lexer2.token (Lexing.from_string str))
  in let checked = type_of_program parsed
     in gen_program checked
    

let prog = "let f = proc (int x, int y) let a = 3 b = 4 f = proc (int x, int y) -(x,y) in +(+(x,y),(f a b)) in (f 5 4)";;

let ast = (Eopl_parser2.program Eopl_lexer2.token (Lexing.from_string prog));;
