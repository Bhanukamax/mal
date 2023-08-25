open Tokenizer
open Types
open Debug_printers

let add_atom_to_list (list : mal list) (atom : mal) = List.rev (atom :: List.rev list)

let rec read_form tokens =
  match tokens with
  | [] -> raise (UNEXPECTED_STATE "Unexpected end of input")
  | LParen :: rest -> read_list rest []
  | token :: rest -> read_atom token, rest

and read_list tokens acc =
  match tokens with
  | [] -> raise UN_TERMINATED_STRING_EXCEPTION
  | RParen :: rest -> MalList (List.rev acc), rest
  | _ ->
    let form, remaining_tokens = read_form tokens in
    read_list remaining_tokens (form :: acc)

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
