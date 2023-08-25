#!/bin/bash
PROG=test
INTERFACE="tokenizer.mli reader.mli"
LINK="tokenizer.ml reader.ml"
BUILD_DIR=_build

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# ocamlc -c tokenizer.mli
# ocamlc -o _build/test tokenizer.ml test.ml
# ocamlc -o _build/reader reader.ml
# "ocamlfind ocamlc -o _build/reader $LINK"

ocamlc -o _build/reader types.ml debug_printers.ml tokenizer.ml reader.ml printer.ml
# ./_build/reader
# ./"$BUILD_DIR"/"$PROG" '(){"Bhan>uka" "1234"}()'
