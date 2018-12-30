(* Type *)
type type_t = Atomictype of string
	      | Proctype of type_t list * type_t
(*	      | Tvartype of int * int array *)

(* Primitive Op *)
type prim = Add | Sub | Mul | Inc | Dec
	    | And | Or | Not | Gt | Ge | Lt | Le | Equal
            | Output | Input

(* Expression *)
type expr = Litexp of int
	    | Trueexp
	    | Falseexp
	    | Varexp of string
	    | Primapp of prim * expr list
	    | Letexp of string list * expr list * expr
	    | Ifexp of expr * expr * expr
	    | Procexp of type_t list * string list * expr
	    | Appexp of expr * expr list
	    | Letrecexp of type_t list * string list * type_t list list * string list list * expr list * expr
type prog = Program of expr

(* Augmented Expression *)
type aexpr = Litexp_A of int
	   | Trueexp_A
	   | Falseexp_A
	   | Varexp_A of string
	   | Primapp_A of prim * aexpr list
	   | Letexp_A of string list * aexpr list * aexpr
	   | Ifexp_A of aexpr * aexpr * aexpr
	   | Procexp_A of type_t list * string list * aexpr * string list (* global names *)
	   | Appexp_A of aexpr * aexpr list
	   | Letrecexp_A of type_t list * string list * type_t list list * string list list * aexpr list * aexpr * string list list (* globalss *)

type aprog = Program_A of aexpr

(* Literal Value *)
type value = Val of int
	     | BoolVal of bool
	     | Closure of string list * aexpr * env
	     | NoVal
(* Value Environment *)
and env = EmptyEnvRecord
	  | ExtendedEnvRecord of string list * value list * env
	  | FunctionBorder of string list * value list * env
	  | RecursivelyExtendedEnvRecord2 of string list * value list * env
	  | RecursivelyExtendedEnvRecord of string list * string list list * aexpr list * env
          | ExtendedEnvRecord_G of string list * value list * env

(* Type Environment *)
and tenv = EmptyTEnvRecord
	 | ExtendedTEnvRecord of string list * (type_t * string list) list * tenv
	 | ExtendedTEnvRecord_F of string list * (type_t * string list) list * tenv
	 | ExtendedTEnvRecord_R of string list * (type_t * string list) list * tenv


		  
		  
