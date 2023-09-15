open Reader
open Types
open Printer
open Env

let read x = Reader.read_str x
let print x = print_endline (Printer.pr_str x)

let debug_env value name env =
  Printf.printf "Defining %s as %s" name (Debug_printers.string_of_mal value);
  print_endline @@ "Env" ^ string_of_env env
;;

let rec eval env ast : mal =
  match ast with
  | MalList { list = [] } -> ast
  | MalList { list = [ MalAtom (Symbol "def!"); MalAtom (Symbol name); value ] } ->
    let value = eval env value in
    let _ = Env.set name value env in
    (* debug_env value name env; *)
    value
  | MalList { list = [ MalAtom (Symbol "let*"); MalList binding; value ] } ->
    let let_env = get_let_binding_env binding.list env in
    let value = eval let_env value in
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
    let found_opt = Env.get sym env in
    found_opt
  | MalList { list = MalFn op :: operand } -> op operand
  | MalList { list; eol } ->
    MalList { list = List.map (eval env) list; eol; listType = List }
  | MalAtom n -> MalAtom n
  | _ -> MalAtom (Number "0")

and get_let_binding_env (bindings : mal list) outer =
  let rec eval_bindings_in_new_env bindings env =
    match bindings with
    | [] -> env
    | MalAtom (Symbol name) :: value :: tail ->
      let _ = Env.set name (eval env value) env in
      eval_bindings_in_new_env tail env
    | _ -> env
  in
  Env.new_env (Some outer) |> eval_bindings_in_new_env bindings
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
    | _ -> raise (UNDEFINED_SYMBOL "? not found")
  in
  Env.set symbol (MalFn (fun a -> num_fun operator (List.rev a))) env
;;

let rec rep () =
  print_string "user> ";
  let env = Env.repl_env in
  let env = num_fold_new "+" env in
  let env = num_fold_new "-" env in
  let env = num_fold_new "/" env in
  let env = num_fold_new "*" env in
  (* print_endline (string_of_env env); *)
  let input = read_line () in
  let _ =
    try input |> read |> eval env |> print with
    | UN_TERMINATED_STRING_EXCEPTION -> print_endline "end of input"
    | ILLEGAL_OPERATION e -> print_endline @@ e ^ " end of input"
    | UNEXPECTED_STATE e -> print_endline @@ e ^ " end of input"
    | UNDEFINED_SYMBOL e -> print_endline e
  in
  rep ()
;;

let _ = rep ()
