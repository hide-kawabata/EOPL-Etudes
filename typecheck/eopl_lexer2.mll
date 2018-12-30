{
(* lexerが利用する変数、関数、型などの定義 *)
open Eopl_parser2
exception Eof
}

(* 正規表現の略記 *)
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
| digit+ (* 整数を字句解析するルール (caml2html: lexer_int) *)
    { INTEGER(int_of_string (Lexing.lexeme lexbuf)) }
| '-' (* -.より後回しにしなくても良い? 最長一致? *)
    { MINUS }
| '+' (* +.より後回しにしなくても良い? 最長一致? *)
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
| lower (digit|lower|upper|'_')* (* 他の「予約語」より後でないといけない *)
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
