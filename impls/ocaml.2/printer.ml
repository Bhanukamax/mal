open Tokenizer
open Types
open Debug_printers

let rec pr_str mal =
  let string_of_token = function
    | LParen -> "("
    | RParen -> ")"
    | LCurly -> "{"
    | RCurly -> "}"
    | LBracket -> "["
    | RBracket -> "]"
    | String s -> "\"" ^ s ^ "\""
    | Symbol "'" -> "quote"
    | Symbol "`" -> "quasiquote"
    | Symbol "~" -> "unquote"
    | Symbol "~@" -> "splice-unquote"
    | Symbol "@" -> "deref"
    | Number s | Symbol s | Keyword s -> s
    | UNKNOWN -> "UNKNOWN"
    | EOF -> "EOF"
    | True -> "true"
    | False -> "false"
    | Nil -> "nil"
  in
  match mal with
  | MalFn _ -> "#<function>"
  | MalAtom atom -> string_of_token atom
  | MalList list ->
    let text = List.map pr_str list.list |> String.concat " " in
    let wrap text = function
      | RCurly -> "{" ^ text ^ "}"
      | RBracket -> "[" ^ text ^ "]"
      | _ -> "(" ^ text ^ ")"
    in
    wrap text list.eol
;;
