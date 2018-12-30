.SUFFIXES: .ml .mly .mll .cmi .mli .cmo

product: gen gen2 rep_loop

rep_loop: mylist.cmo eopl_parser.cmo eopl_lexer.cmo eopl.cmo syntax.cmo frontend.cmo
	ocamlc -o rep_loop mylist.cmo eopl_parser.cmo eopl_lexer.cmo eopl.cmo syntax.cmo frontend.cmo

# for '#use'
use: eopl_lexer.ml
	ocamlc -i eopl_lexer.ml > eopl_lexer.mli

top:
#	ocamlmktop -custom -o eopl_top mylist.cmo eopl_parser.cmo eopl_lexer.cmo eopl.cmo syntax.cmo
	ocamlmktop -custom -o gen_top mylist.cmo eopl_parser.cmo eopl_lexer.cmo codegen.cmo syntax.cmo

gen: eopl_parser.cmo eopl_lexer.cmo syntax.cmo codegen.cmo gen_frontend.cmo
	ocamlc -o gen mylist.cmo eopl_parser.cmo eopl_lexer.cmo syntax.cmo codegen.cmo gen_frontend.cmo

gen2: eopl_parser2.cmo eopl_lexer2.cmo syntax2.cmo codegen2.cmo gen2_frontend.cmo
	ocamlc -o gen2 mylist.cmo eopl_parser2.cmo eopl_lexer2.cmo syntax2.cmo codegen2.cmo gen2_frontend.cmo

eopl_parser2.cmo: eopl_parser2.ml eopl_parser2.cmi syntax2.cmi
	ocamlc -c eopl_parser2.ml

eopl_parser.cmo: eopl_parser.ml eopl_parser.cmi syntax.cmi
	ocamlc -c eopl_parser.ml

eopl_parser2.cmi: eopl_parser2.mli syntax2.cmi
	ocamlc -c eopl_parser2.mli

eopl_parser.cmi: eopl_parser.mli syntax.cmi
	ocamlc -c eopl_parser.mli

codegen2.cmo: codegen2.ml mylist.cmo inst.cmo
	ocamlc -c codegen2.ml mylist.cmo inst.cmo

codegen.cmo: codegen.ml mylist.cmo inst.cmo
	ocamlc -c codegen.ml mylist.cmo inst.cmo


.ml.cmo:
	ocamlc -c $<

.ml.cmi:
	ocamlc -c $<

.mly.mli:
	ocamlyacc $<

.mly.ml:
	ocamlyacc $<

.mll.ml:
	ocamllex $<

clean:
	rm -f *~ *.cmo *.cmi *.mli *.cmx \
               gen gen2 \
               eopl_parser.ml eopl_parser2.ml \
               eopl_lexer.ml eopl_lexer2.ml \
               rep_loop eopl_top gen_top gen
