%{
open Syntax2
(* open Mylist *)
%}

%token <int> INTEGER
%token <string> IDENT
%token LET
%token IN
%token LPAR
%token RPAR
%token COMMA
%token EQUAL
%token PLUS
%token MINUS
%token AST
%token INC
%token DEC
%token IF
%token THEN
%token ELSE
%token PROC
%token REC
%token INPUT
%token OUTPUT

/* types */
%token INT
%token BOOL
%token ARROW
%token TRUE
%token FALSE
%token AND
%token OR 
%token NOT
%token GT 
%token GE 
%token LT 
%token LE 
%token EQUAL2


%token EOE

%start program

%type <Syntax2.prog> program
/*
%type <expr> expression
%type <prim> primitive
%type <expr list> expr_list
%type <string list * expr list> ident_expr_list
*/

%%

program
/*: expression EOE { Program $1 }*/
: expression { Program $1 }

type_expr
: INT { Atomictype "int" }
| BOOL { Atomictype "bool"  }
| LPAR type_expr_list ARROW type_expr RPAR
      { Proctype ($2, $4) }

type_expr_list
: { [] }
| type_expr_list_1 { $1 }

type_expr_list_1
: type_expr { $1::[] }
| type_expr AST type_expr_list_1 { $1::$3 }


expression
: INTEGER { Litexp $1 }
| TRUE { Trueexp }
| FALSE { Falseexp }
| IDENT { Varexp $1 }
| primitive LPAR expr_list RPAR { Primapp ($1, $3) }
| LET ident_expr_list IN expression { let (ids, rands) = $2 in Letexp (ids, rands, $4) }
| IF expression THEN expression ELSE expression { Ifexp ($2, $4, $6) }
| PROC LPAR id_type_list RPAR expression
      { let (texps, ids) = $3 in Procexp (texps, ids, $5) }
| LPAR expression expr_list_nocomma RPAR { Appexp ($2, $3) }
| LET REC recproc_list IN expression
      { let (res_texp, proc_names, arg_texps, idss, bodies) = $3
	in Letrecexp (res_texp, proc_names, arg_texps, idss, bodies, $5) }

recproc_list
: { ([], [], [], [], []) }
| recproc_list_1 { $1 }

recproc_list_1
: type_expr IDENT LPAR id_type_list RPAR EQUAL expression
   { let (arg_texps, ids) = $4 in ($1::[], $2::[], arg_texps::[], ids::[], $7::[]) }
| type_expr IDENT LPAR id_type_list RPAR EQUAL expression recproc_list_1
    { let (res_texps, proc_names, arg_texpss, idss, bodies) = $8
      in let (arg_texps, ids) = $4
      in ($1::res_texps, $2::proc_names, arg_texps::arg_texpss, ids::idss, $7::bodies) }

expr_list_nocomma
: { [] }
| expression expr_list_nocomma { $1::$2 }
    
/*
expr_list_nocomma_1
: expression { $1::[] }
| expression expr_list_nocomma_1 { $1::$2 }
*/
    
id_type_list
: { ([], []) }
| id_type_list_1 { $1 }

id_type_list_1
: type_expr IDENT { ($1::[], $2::[]) }
| type_expr IDENT COMMA id_type_list_1
    { let (texps, ids) = $4 in ($1::texps, $2::ids) }

expr_list
: { [] }
| expr_list_1 { $1 }

expr_list_1
: expression { $1::[] }
| expression COMMA expr_list_1 { $1::$3 }

ident_expr_list
: { ([], []) }
| ident_expr_list_1 { $1 }

ident_expr_list_1
: IDENT EQUAL expression { ($1::[], $3::[]) }
| IDENT EQUAL expression ident_expr_list_1
      { let (ids, rands) = $4 in ($1::ids, $3::rands) }

primitive
: PLUS { Add }
| MINUS { Sub }
| AST { Mul }
| INC { Inc }
| DEC { Dec }
| AND { And }
| OR { Or }
| NOT { Not }
| GT { Gt }
| GE { Ge }
| LT { Lt }
| LE { Le }
| EQUAL2 { Equal }
| OUTPUT { Output }
| INPUT { Input }
