open Tokenizer
open Types
open Debug_printers

let rec pr_str mal =
  let string_of_token = function
    | LParen -> "("
    | RParen -> ")"
    | String s -> "\"" ^ s ^ "\""
    | Number s | Symbol s -> s
    | UNKNOWN -> "UNKNOWN"
    | EOF -> "EOF"
  in
  match mal with
  | MalAtom atom -> string_of_token atom
  | MalList list ->
    let text = List.map pr_str list |> String.concat " " in
    "(" ^ text ^ ")"
;;
