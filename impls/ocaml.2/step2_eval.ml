open Reader
open Types
open Printer

let read x = Reader.read_str x
let print x = print_endline (Printer.pr_str x)

module Env = Map.Make (String)

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
    let found_opt = Env.find_opt sym env in
    (match found_opt with
     | None -> raise (ILLEGAL_OPERATION ("undefined symbol " ^ sym))
     | Some fn -> MalFn fn)
  | MalList { list = MalFn op :: operand } -> op operand
  | MalList { list; eol } -> MalList { list = List.map (eval env) list; eol }
  | MalAtom n -> MalAtom n
  | _ -> MalAtom (Number "0")
;;

let num_fun op = function
  | [ MalAtom (Number a); MalAtom (Number b) ] ->
    MalAtom (Number (string_of_int (op (int_of_string b) (int_of_string a))))
  | _ -> MalAtom (Number "1")
;;

let num_fold op acc el = num_fun op [ acc; el ]

let num_fold_new symbol =
  let operator =
    match symbol with
    | "+" -> ( + )
    | "-" -> ( - )
    | "*" -> ( * )
    | "/" -> ( / )
    | _ -> raise (ILLEGAL_OPERATION "unknown operator")
  in
  Env.add symbol (fun a -> num_fun operator (List.rev a))
;;

(* Env.add symbol (fun a -> *)
(*   List.fold_left (num_fold operator) (MalAtom (Number "0")) (List.rev a)) *)

let rec rep () =
  print_string "user> ";
  let env =
    Env.empty
    |> num_fold_new "+"
    |> num_fold_new "-"
    |> num_fold_new "/"
    |> num_fold_new "*"
    (* |> Env.add "-" (fun a -> *)
    (*      List.fold_left (num_fold ( - )) (MalAtom (Number "0")) (List.rev a)) *)
    (* |> Env.add "*" (fun a -> *)
    (*      List.fold_left (num_fold ( - )) (MalAtom (Number "0")) (List.rev a)) *)
    (* |> Env.add "/" (fun a -> *)
    (*      List.fold_left (num_fold ( - )) (MalAtom (Number "0")) (List.rev a)) *)
  in
  let input = read_line () in
  let _ =
    try input |> read |> eval env |> print with
    | UN_TERMINATED_STRING_EXCEPTION -> print_endline "end of input"
    | ILLEGAL_OPERATION _ -> print_endline "end of input"
    | UNEXPECTED_STATE _ -> print_endline "end of input"
  in
  rep ()
;;

let _ = rep ()

(*
and evaluate ast =
  match ast with
  | MalAtom (Number n) -> MalAtom (Number n) (* Numeric atom, return as-is *)
  | MalList { list = []; eol = _ } ->
    MalAtom (Number "0") (* Empty list evaluates to 0.0, adjust as needed *)
  | MalList { list = operator :: operands; eol = _ } ->
    (match operator with
     | MalAtom (Symbol "+") ->
       let result =
         List.fold_left
           ( + )
           0
           (List.map
              (fun op ->
                match evaluate op with
                | MalAtom (Number n) -> int_of_string n
                | _ -> 0)
              operands)
       in
       MalAtom (Number (string_of_int result))
     | MalAtom (Symbol "-") ->
       (match operands with
        | [] -> MalAtom (Number "0") (* Handle unary minus, e.g., (- 5) => -5 *)
        | hd :: tl ->
          let result =
            List.fold_left
              ( - )
              (match evaluate hd with
               | MalAtom (Number n) -> int_of_string n
               | _ -> 0)
              (List.map
                 (fun op ->
                   match evaluate op with
                   | MalAtom (Number n) -> int_of_string n
                   | _ -> 0)
                 tl)
          in
          MalAtom (Number (string_of_int result)))
     | MalAtom (Symbol "*") ->
       let result =
         List.fold_left
           ( * )
           1
           (List.map
              (fun op ->
                match evaluate op with
                | MalAtom (Number n) -> int_of_string n
                | _ -> 1)
              operands)
       in
       MalAtom (Number (string_of_int result))
     | MalAtom (Symbol "/") ->
       (match operands with
        | [] -> raise (ILLEGAL_OPERATION "Division by zero")
        | hd :: tl ->
          let result =
            List.fold_left
              ( / )
              (match evaluate hd with
               | MalAtom (Number n) -> int_of_string n
               | _ -> 0)
              (List.map
                 (fun op ->
                   match evaluate op with
                   | MalAtom (Number n) -> int_of_string n
                   | _ -> 1)
                 tl)
          in
          MalAtom (Number (string_of_int result)))
     | _ -> raise (ILLEGAL_OPERATION "Unsupported operator"))
  | _ -> raise (ILLEGAL_OPERATION "Invalid expression")
;;
*)
