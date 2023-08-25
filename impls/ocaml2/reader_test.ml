open OUnit2
open Types
open Reader
open Debug_printers

let mal_assert result expected = assert_equal result expected ~printer:string_of_mal

let mal_test_one _ =
  let got = read_str "(1 2)" in
  let expected = MalList [ MalList [ MalAtom (Number "1"); MalAtom (Number "2") ] ] in
  mal_assert expected got
;;

let mal_test_two _ =
  let got = read_str "(1 2 (3 4))" in
  let expected =
    MalList
      [ MalList
          [ MalAtom (Number "1")
          ; MalAtom (Number "2")
          ; MalList [ MalAtom (Number "3"); MalAtom (Number "4") ]
          ]
      ]
  in
  mal_assert expected got
;;

let test_top_level_one_atom _ =
  let got = read_str "1" in
  let expected = MalList [ MalAtom (Number "1") ] in
  mal_assert expected got
;;

let test_top_level_two_atoms _ =
  let got = read_str "1 2" in
  let expected = MalList [ MalAtom (Number "1"); MalAtom (Number "2") ] in
  mal_assert expected got
;;

let test_top_level_three_atoms _ =
  let got = read_str "1 2 3" in
  let expected =
    MalList [ MalAtom (Number "1"); MalAtom (Number "2"); MalAtom (Number "3") ]
  in
  mal_assert expected got
;;

let test_top_level_four_atoms _ =
  let got = read_str "1 2 3 44" in
  let expected =
    MalList
      [ MalAtom (Number "1")
      ; MalAtom (Number "2")
      ; MalAtom (Number "3")
      ; MalAtom (Number "44")
      ]
  in
  mal_assert expected got
;;

let test_nothing _ =
  let got = read_str "" in
  let expected = MalList [] in
  mal_assert expected got
;;

let empty_sexp _ =
  let got = read_str "()" in
  let expected = MalList [ MalList [] ] in
  mal_assert expected got
;;

let two_empty_sexp _ =
  let got = read_str "()()" in
  let expected = MalList [ MalList []; MalList [] ] in
  mal_assert expected got
;;

let one_atom_sexp _ =
  let got = read_str "(1)" in
  let expected = MalList [ MalAtom (Number "1") ] in
  mal_assert expected got
;;

let test_suite =
  "test suite for sum"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 0)
       ; "top level one atom" >:: test_top_level_one_atom
       ; "top level two atoms" >:: test_top_level_two_atoms
       ; "top level three atoms" >:: test_top_level_three_atoms
       ; "top level four atoms" >:: test_top_level_four_atoms
       ; "nothing should produce and emtpy list" >:: test_nothing
       ; "empty sexp" >:: empty_sexp
       ; "two empy sexp" >:: two_empty_sexp
         (* ; "one atom sexp" >:: one_atom_sexp *)
         (* ; "tokenize and parse (1 2)" >:: mal_test_one *)
         (* ; "tokenize and parse (1 2 (3 4))" >:: mal_test_two *)
       ]
;;

let one_atom _ =
  let tokens = [ Number "1" ] in
  let got, _ = read_form tokens in
  let expected = MalList [ MalAtom (Number "1") ] in
  mal_assert expected got
;;

let mal_two_atom _ =
  let tokens = [ Number "1" ] in
  let got, _ = read_form tokens in
  let expected = MalList [ MalAtom (Number "1") ] in
  mal_assert expected got
;;

let mal_many_atom _ =
  let tokens =
    [ Number "1"
    ; Number "2"
    ; Number "3"
    ; Number "4"
    ; Number "5"
    ; Number "6"
    ; Number "7"
    ; Number "8"
    ; Number "9"
    ]
  in
  let got, _ = read_form tokens in
  let expected =
    MalList
      [ MalAtom (Number "1")
      ; MalAtom (Number "2")
      ; MalAtom (Number "3")
      ; MalAtom (Number "4")
      ; MalAtom (Number "5")
      ; MalAtom (Number "6")
      ; MalAtom (Number "7")
      ; MalAtom (Number "8")
      ; MalAtom (Number "9")
      ]
  in
  mal_assert expected got
;;

let empty_sexp _ =
  let tokens = [ LParen; RParen ] in
  let got, _ = read_form tokens in
  let expected = MalList [ MalList [] ] in
  mal_assert expected got
;;

let two_empty_sexp _ =
  let tokens = [ LParen; RParen; LParen; RParen ] in
  let got, _ = read_form tokens in
  let expected = MalList [ MalList []; MalList [] ] in
  mal_assert expected got
;;

let sexp_one_item _ =
  let tokens = [ Number "1" ] in
  let got, _ = read_form tokens in
  let expected = MalAtom (Number "1") in
  mal_assert expected got
;;

let mal_test_suite =
  "test suite for sum"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 0)
         (* ; "top level one atom" >:: one_atom *)
         (* ; "top level one atom" >:: empty_sexp *)
         (* ; "top level one atom" >:: two_empty_sexp *)
         (* ; "top level one atom" >:: mal_many_atom *)
       ; "top level one atom" >:: sexp_one_item
         (* ; "one atom sexp" >:: one_atom_sexp *)
         (* ; "tokenize and parse (1 2)" >:: mal_test_one *)
         (* ; "tokenize and parse (1 2 (3 4))" >:: mal_test_two *)
       ]
;;

(* let _ = run_test_tt_main test_suite *)
let _ = run_test_tt_main mal_test_suite
