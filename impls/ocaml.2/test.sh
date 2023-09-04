#!/bin/bash
# rm -rf ./_build/test_tokenizer
# ocamlfind ocamlc -o _build/test_tokenizer -I +ounit -linkpkg -package ounit2 tokenizer.ml test_tokenizer.ml
# ./_build/test_tokenizer

# export DEPS=tokenizer.ml reader.ml
# export TEST_FILE=reader_test
rm -rf ./_build/test_reader
# "ocamlfind ocamlc -o _build/$TEST_FILE -I +ounit -linkpkg -package ounit2 $DEPS $TEST_FILE.ml"
ocamlfind ocamlc -o _build/reader_test -I +ounit -linkpkg -package ounit2 types.ml debug_printers.ml tokenizer.ml reader.ml reader_test.ml

./_build/reader_test

# "./_build/$TEST_FILE"

