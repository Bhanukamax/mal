open Tokenizer
open Types
open Debug_printers

let add_atom_to_list (list : mal list) (atom : mal) = List.rev (atom :: List.rev list)

let rec read_form tokens =
  let add_quote quote rest =
    let form, rest' = read_form rest in
    MalList { list = [ MalAtom (Symbol quote); form ]; eol = RParen }, rest'
  in
  match tokens with
  | [] -> raise (UNEXPECTED_STATE "Unexpected end of input")
  | LParen :: rest -> read_list RParen rest []
  | LCurly :: rest -> read_list RCurly rest []
  | LBracket :: rest -> read_list RBracket rest []
  | Symbol "'" :: rest -> add_quote "'" rest
  | Symbol "`" :: rest -> add_quote "`" rest
  | Symbol "~" :: rest -> add_quote "~" rest
  | Symbol "~@" :: rest -> add_quote "~@" rest
  | Symbol "@" :: rest -> add_quote "@" rest
  | token :: rest -> read_atom token, rest

and read_list eol tokens acc =
  match tokens with
  | [] -> raise UN_TERMINATED_STRING_EXCEPTION
  | t :: rest when t = eol -> MalList { eol = t; list = List.rev acc }, rest
  | _ ->
    let form, remaining_tokens = read_form tokens in
    read_list eol remaining_tokens (form :: acc)

and read_atom token =
  match token with
  | Number s -> MalAtom (Number s)
  | String s -> MalAtom (String s)
  | Symbol s -> MalAtom (Symbol s)
  | _ -> raise (ILLEGAL_OPERATION "Invalid token")
;;

let read_str text =
  let mal, _ = text |> tokenize |> read_form in
  mal
;;
