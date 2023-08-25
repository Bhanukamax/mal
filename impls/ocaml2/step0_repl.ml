let read x = x
let eval x = x
let print x = print_endline x

let rec rep () =
  print_string "user> ";
  let input = read_line () in
  input |> read |> eval |> print;
  rep ()
;;

let _ = rep ()
