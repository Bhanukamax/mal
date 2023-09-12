open Reader
open Types
open Printer
open Env

let read x = Reader.read_str x
let print x = print_endline (Printer.pr_str x)

let rec eval env ast =
  match ast with
  | MalList { list = [] } -> ast
  | MalList _ ->
    let result = eval_ast env ast in
    (match result with
     | MalList list -> eval_ast env (MalList list)
     | _ -> result)
  | _ -> eval_ast env ast

and eval_ast env ast =
  match ast with
  | MalAtom (Symbol sym) ->
    let found_opt = Env.get sym env in
    found_opt
    (* (match found_opt with *)
    (*  | None -> raise (ILLEGAL_OPERATION ("undefined symbol " ^ sym)) *)
    (*  | Some fn -> MalFn fn) *)
  | MalList { list = MalFn op :: operand } -> op operand
  | MalList { list; eol } ->
    MalList { list = List.map (eval env) list; eol; listType = List }
  | MalAtom n -> MalAtom n
  | _ -> MalAtom (Number "0")
;;

let num_fun op = function
  | [ MalAtom (Number a); MalAtom (Number b) ] ->
    MalAtom (Number (string_of_int (op (int_of_string b) (int_of_string a))))
  | _ -> MalAtom (Number "1")
;;

let num_fold op acc el = num_fun op [ acc; el ]

let num_fold_new symbol env =
  let operator =
    match symbol with
    | "+" -> ( + )
    | "-" -> ( - )
    | "*" -> ( * )
    | "/" -> ( / )
    | _ -> raise (ILLEGAL_OPERATION "unknown operator")
  in
  Env.set symbol (MalFn (fun a -> num_fun operator (List.rev a))) env
;;

let rec rep () =
  print_string "user> ";
  let env = Env.new_env in
  let env = num_fold_new "+" env in
  let env = num_fold_new "-" env in
  let env = num_fold_new "/" env in
  let env = num_fold_new "*" env in
  let input = read_line () in
  let _ =
    try input |> read |> eval env |> print with
    | UN_TERMINATED_STRING_EXCEPTION -> print_endline "end of input"
    | ILLEGAL_OPERATION e -> print_endline @@ e ^ " end of input"
    | UNEXPECTED_STATE e -> print_endline @@ e ^ " end of input"
  in
  rep ()
;;

let _ = rep ()
