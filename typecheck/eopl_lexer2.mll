{
(* lexer�����Ѥ����ѿ����ؿ������ʤɤ���� *)
open Eopl_parser2
exception Eof
}

(* ����ɽ����ά�� *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| '('
    { LPAR }
| ')'
    { RPAR }
| digit+ (* �����������Ϥ���롼�� (caml2html: lexer_int) *)
    { INTEGER(int_of_string (Lexing.lexeme lexbuf)) }
| '-' (* -.����󤷤ˤ��ʤ��Ƥ��ɤ�? ��Ĺ����? *)
    { MINUS }
| '+' (* +.����󤷤ˤ��ʤ��Ƥ��ɤ�? ��Ĺ����? *)
    { PLUS }
| '*'
    { AST }
| '='
    { EQUAL }
| "let"
    { LET }
| "in"
    { IN }
| ','
    { COMMA }
| "add1"
    { INC }
| "sub1"
    { DEC }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "proc"
    { PROC }
| "rec"
    { REC }
| "->"
    { ARROW }
| "true"
    { TRUE }
| "false"
    { FALSE }
| "int" { INT }
| "bool" { BOOL }
| "<" { LT }
| "<=" { LE }
| ">" { GT }
| ">=" { GE }
| "==" { EQUAL2 }
| "not" { NOT }
| "&&" { AND }
| "||" { OR }
| ";;"
    { EOE }
| eof
    { raise Eof }
| "input" { INPUT }
| "output" { OUTPUT }
| lower (digit|lower|upper|'_')* (* ¾�Ρ�ͽ���פ���Ǥʤ��Ȥ����ʤ� *)
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
