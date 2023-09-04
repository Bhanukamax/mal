open Types

let append_to_string append string = string ^ append
let prepend_to_string prepend string = prepend ^ string

let string_of_token = function
  | LParen -> "LParen "
  | RParen -> "RParen"
  | LCurly -> "LCurly "
  | RCurly -> "RCurly"
  | Number s -> "Number (" ^ s ^ ")"
  | String s -> "String (" ^ s ^ ")"
  | Symbol s -> "Symbol (" ^ s ^ ")"
  | UNKNOWN -> "UNKNOWN"
  | EOF -> "EOF"
;;

let string_of_token_list tokens =
  let text = tokens |> List.map string_of_token |> String.concat "\n;" in
  "[\n" ^ text ^ "\n]"
;;

let print_tokens tokens =
  print_endline "THE TOKENS: ";
  tokens |> List.map string_of_token |> String.concat "\n;" |> print_endline
;;

let string_of_mal mal =
  let rec build_string_of_mal level = function
    | MalAtom t -> "MalAtom (" ^ string_of_token t ^ ")"
    | MalList { list } ->
      let string_of_mal_list (list_string : string) : string =
        (* let tabs = String.make level '\t' in *)
        String.concat "" [ "MalList"; "\n"; "["; list_string; "]" ]
      in
      list
      |> List.map (fun a -> build_string_of_mal (level + 1) a)
      |> String.concat "; "
      |> string_of_mal_list
  in
  build_string_of_mal 1 mal
  |> prepend_to_string "\n********** \n"
  |> append_to_string "\n********** \n"
;;

let print_mal (mal : mal) =
  mal |> string_of_mal |> append_to_string "<<<<< \n\n" |> print_endline
;;
