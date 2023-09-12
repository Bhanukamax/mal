open OUnit2
open Tokenizer
open Types
open Debug_printers

let assert_tokens result expected =
  assert_equal result expected ~printer:string_of_token_list
;;

let tokenize_three _ =
  let got = Tokenizer.tokenize "1 -11 \"one\"" in
  let expected = [ Number "1"; Number "-11"; String "one" ] in
  assert_tokens expected got
;;

let tokenize_one _ =
  let got = Tokenizer.tokenize "(+ 1 11)" in
  let expected = [ LParen; Symbol "+"; Number "1"; Number "11"; RParen ] in
  assert_tokens expected got
;;

let tokenize_def _ =
  let got = Tokenizer.tokenize "(def! foo (+ 1 11))" in
  let expected =
    [ LParen
    ; Symbol "def!"
    ; Symbol "foo"
    ; LParen
    ; Symbol "+"
    ; Number "1"
    ; Number "11"
    ; RParen
    ; RParen
    ]
  in
  assert_tokens expected got
;;

let tokenizer_test_suit =
  "test suite for sum"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 0)
         (* ; "top level one atom" >:: one_atom *)
         (* ; "top level one atom" >:: empty_sexp *)
         (* ; "top level one atom" >:: two_empty_sexp *)
         (* ; "top level one atom" >:: mal_many_atom *)
         (* ; "top level one atom" >:: tokenize_one *)
         (* ; "top level one atom" >:: tokenize_two *)
       ; "top level one atom" >:: tokenize_one
       ; "top level one atom" >:: tokenize_three
       ; "top level one atom" >:: tokenize_def
         (* ; "one atom sexp" >:: one_atom_sexp *)
         (* ; "tokenize and parse (1 2)" >:: mal_test_one *)
         (* ; "tokenize and parse (1 2 (3 4))" >:: mal_test_two *)
       ]
;;

let _ = run_test_tt_main tokenizer_test_suit
