product: byte-code gen rep_loop

byte-code:
	ocamlc -c mylist.ml
	ocamllex eopl_lexer.mll   # eopl_lexer.ml
	ocamlyacc eopl_parser.mly # eopl_parser.ml, eopl_parser.mli
	ocamlc -c syntax.ml  # syntax.cmi, syntax.cmo
	ocamlc -c eopl_parser.mli # eopl_parser.cmi
	ocamlc -c eopl_lexer.ml   # eopl_lexer.cmi, eopl_lexer.cmo
	ocamlc -c eopl_parser.ml  # eopl_parser.cmo
	ocamlc -c eopl.ml
	ocamlc -c frontend.ml
	ocamlc -c inst.ml # inst.cmi, inst.cmo
	ocamlc -c codegen.ml # codegen.cmi, codegen.cmo
	ocamlc -c gen_frontend.ml

rep_loop: byte-code
	ocamlc -o rep_loop mylist.cmo eopl_parser.cmo eopl_lexer.cmo eopl.cmo syntax.cmo frontend.cmo

# for '#use'
use: eopl_lexer.ml
	ocamlc -i eopl_lexer.ml > eopl_lexer.mli

top:
#	ocamlmktop -custom -o eopl_top mylist.cmo eopl_parser.cmo eopl_lexer.cmo eopl.cmo syntax.cmo
	ocamlmktop -custom -o gen_top mylist.cmo eopl_parser.cmo eopl_lexer.cmo codegen.cmo syntax.cmo

gen: byte-code
	ocamlc -o gen mylist.cmo eopl_parser.cmo eopl_lexer.cmo syntax.cmo codegen.cmo gen_frontend.cmo

clean:
	rm -f *~ mylist.cmi mylist.cmo \
              eopl_parser.cmi eopl_parser.cmo eopl_parser.cmx eopl_parser.ml eopl_parser.mli \
              eopl_lexer.cmi eopl_lexer.cmo eopl_lexer.cmx eopl_lexer.ml eopl_lexer.mli \
              syntax.cmi syntax.cmo syntax.cmx syntax.mli \
              eopl.cmi eopl.cmo eopl.cmx eopl.mli \
              frontend.cmi frontend.cmo frontend.cmx frontend.mli \
              gen_frontend.cmi gen_frontend.cmo \
              codegen.cmi codegen.cmo \
              inst.cmi inst.cmo \
              rep_loop eopl_top gen_top gen
