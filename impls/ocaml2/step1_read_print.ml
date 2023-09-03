open Reader
open Types
open Printer

let read x = Reader.read_str x
let eval x = x
let print x = print_endline (Printer.pr_str x)

let rec rep () =
  print_string "user> ";
  let input = read_line () in
  (* print_endline input; *)
  let _ =
    try input |> read |> eval |> print with
    | UN_TERMINATED_STRING_EXCEPTION -> print_endline "end of input"
    | ILLEGAL_OPERATION _ -> print_endline "end of input"
  in
  rep ()
;;

let _ = rep ()
