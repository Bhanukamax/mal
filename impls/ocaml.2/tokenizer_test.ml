open OUnit2
open Types
open Tokenizer
open Debug_printers

let mal_assert (result : token list) (expected : token list) =
  assert_equal result expected ~printer:(fun a ->
    List.map string_of_token a |> String.concat "\n; " |> fun a -> "\n[" ^ a ^ "]")
;;

let mal_test_one _ =
  let got = tokenize "(1 2)" in
  let expected = [ LParen; Number "1"; Number "2"; RParen ] in
  mal_assert expected got
;;

let test_multiple_sexp _ =
  let got = tokenize "(1 2)(22 33)" in
  let expected =
    [ LParen; Number "1"; Number "2"; RParen; LParen; Number "22"; Number "33"; RParen ]
  in
  mal_assert expected got
;;

let nested_sexp _ =
  let got = tokenize "(1 2(3 4) 5 6)(22 33)" in
  let expected =
    [ LParen
    ; Number "1"
    ; Number "2"
    ; LParen
    ; Number "3"
    ; Number "4"
    ; RParen
    ; Number "5"
    ; Number "6"
    ; RParen
    ; LParen
    ; Number "22"
    ; Number "33"
    ; RParen
    ]
  in
  mal_assert expected got
;;

let test_suite =
  "test suite for sum"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 0)
       ; "top level one atom" >:: mal_test_one
       ; "top level one atom" >:: test_multiple_sexp
       ; "top level one atom" >:: nested_sexp
         (* ; "top level two atoms" >:: test_top_level_two_atoms *)
         (* ; "top level three atoms" >:: test_top_level_three_atoms *)
         (* ; "top level four atoms" >:: test_top_level_four_atoms *)
         (* ; "nothing should produce and emtpy list" >:: test_nothing *)
         (* ; "empty sexp" >:: empty_sexp *)
         (* ; "one atom sexp" >:: one_atom_sexp *)
         (* ; "tokenize and parse (1 2)" >:: mal_test_one *)
         (* ; "tokenize and parse (1 2 (3 4))" >:: mal_test_two *)
       ]
;;

let _ = run_test_tt_main test_suite
