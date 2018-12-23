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

(* Literal Value *)
type value = Val of int
	     | BoolVal of bool
	     | Closure of string list * expr * env
	     | NoVal
(* Value Environment *)
and env = EmptyEnvRecord
	  | ExtendedEnvRecord of string list * value list * env
	  | FunctionBorder of string list * value list * env
	  | RecursivelyExtendedEnvRecord2 of string list * value list * env
	  | RecursivelyExtendedEnvRecord of string list * string list list * expr list * env

(* Type Environment *)
and tenv = EmptyTEnvRecord
	   | ExtendedTEnvRecord of string list * type_t list * tenv


		  
		  
