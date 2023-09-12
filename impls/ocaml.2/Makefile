OCAMLC = ocamlc

TARGETS = step0_repl step1_read_print step2_eval
TARGETS_STEP3 = step3_env
ALL_TARGETS = $(TARGETS) $(TARGETS_STEP3)
MODULES = types.ml debug_printers.ml tokenizer.ml reader.ml printer.ml
MODULES_STEP3 = $(MODULES) env.ml

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

test:
	rm -rf ./_build/test_reader
	ocamlfind ocamlc -o _build/reader_test -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml reader_test.ml
	./_build/reader_test

test.t:
	rm -rf ./_build/tokenizer_test
	ocamlfind ocamlc -o _build/tokenizer_test -I +ounit -linkpkg -package ounit2,str types.ml debug_printers.ml tokenizer.ml reader.ml tokenizer_test.ml
	./_build/tokenizer_test

