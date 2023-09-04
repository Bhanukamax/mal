OCAMLC = ocamlc

TARGETS = step0_repl step1_read_print

step0_repl: clean
	$(OCAMLC) -o step0_repl types.ml debug_printers.ml tokenizer.ml reader.ml printer.ml step0_repl.ml
step1_read_print: clean
	$(OCAMLC) -o step1_read_print types.ml debug_printers.ml tokenizer.ml reader.ml printer.ml step1_read_print.ml

build:
	./build.sh

test:
	rm -rf ./_build/test_reader
	ocamlfind ocamlc -o _build/reader_test -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml reader_test.ml
	./_build/reader_test

test.t:
	rm -rf ./_build/tokenizer_test
	ocamlfind ocamlc -o _build/tokenizer_test -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml tokenizer_test.ml
	./_build/tokenizer_test

clean:
	rm -rf $(TARGETS)
	rm -rf ./*.cmo
	rm -rf ./*.cmi
