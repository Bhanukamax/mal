open Reader
open Types
open Printer
open Env

let debug_env env sym is_debug_env =
  if is_debug_env == true then print_endline ("looking for: " ^ sym);
  print_endline (string_of_env env)
;;

let read x = Reader.read_str x
let print x = print_endline (Printer.pr_str x)

let rec eval env ast : mal =
  match ast with
  | MalList { list = [] } -> ast
  | MalList { list = [ MalAtom (Symbol "def!"); MalAtom (Symbol name); value ] } ->
    let value = eval env value in
    let _ = Env.set name value env in
    value
  | MalList { list = [ MalAtom (Symbol "let*"); MalList bindingList; value ] } ->
    let value = eval env value in
    value
  | MalList _ ->
    let result = eval_ast env ast in
    let ast =
      match result with
      | MalList list -> eval_ast env (MalList list)
      | _ -> result
    in
    ast
  | _ -> eval_ast env ast

and eval_ast env ast : mal =
  match ast with
  | MalAtom (Symbol sym) ->
    (* debug_env env sym false; *)
    let found_opt = Env.get sym env in
    found_opt
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
