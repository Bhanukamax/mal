OCAMLC = ocamlc

TARGETS = step0_repl step1_read_print step2_eval
TARGETS_STEP3 = step3_env step4_if_fn_do
ALL_TARGETS = $(TARGETS) $(TARGETS_STEP3)
MODULES = types.ml debug_printers.ml tokenizer.ml reader.ml printer.ml
MODULES_STEP3 = $(MODULES) env.ml core.ml

all: $(TARGETS)

$(TARGETS): %: %.ml
	$(OCAMLC) -o $@ $(MODULES) $<

$(TARGETS_STEP3): %: %.ml
	$(OCAMLC) -o $@ $(MODULES_STEP3) $<


clean:
	rm -rf $(ALL_TARGETS)
	rm -rf ./*.cmo
	rm -rf ./*.cmi

build:
	./build.sh

test_tokenize:
	rm -rf ./_build/test_tokenize
	ocamlfind ocamlc -o _build/test_tokenize -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml test_tokenize.ml
	./_build/test_tokenize

test_reader:
	rm -rf ./_build/test_reader
	ocamlfind ocamlc -o _build/test_reader -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml test_reader.ml
	./_build/test_reader

test:
	rm -rf ./_build/test_reader
	ocamlfind ocamlc -o _build/reader_test -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml reader_test.ml
	./_build/reader_test

test.t:
	rm -rf ./_build/tokenizer_test
	ocamlfind ocamlc -o _build/tokenizer_test -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml tokenizer_test.ml
	./_build/tokenizer_test

