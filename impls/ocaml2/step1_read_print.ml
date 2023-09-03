open Reader
open Printer

let read x = Reader.read_str x
let eval x = x
let print x = print_endline (Printer.pr_str x)

let rec rep () =
  print_string "user> ";
  let input = read_line () in
  (* print_endline input; *)
  input |> read |> eval |> print;
  rep ()
;;

let _ = rep ()
