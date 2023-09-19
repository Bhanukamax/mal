open Types

let append_to_string append string = string ^ append
let prepend_to_string prepend string = prepend ^ string

let string_of_token = function
  | LParen -> "LParen "
  | RParen -> "RParen"
  | LCurly -> "LCurly "
  | RCurly -> "RCurly"
  | LBracket -> "LBracket "
  | RBracket -> "RBracket "
  | Number s -> "Number (\"" ^ s ^ "\")"
  | String s -> "String (\"" ^ s ^ "\")"
  | Symbol s -> "Symbol (\"" ^ s ^ "\")"
  | Keyword s -> "Keyword (\"" ^ s ^ "\")"
  | UNKNOWN -> "UNKNOWN"
  | EOF -> "EOF"
  | True -> "true"
  | False -> "false"
  | Nil -> "nil"
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
    | MalFn _ -> "MalFn"
    | MalAtom t -> "MalAtom (" ^ string_of_token t ^ ")"
    | MalList { list; eol } ->
      let string_of_mal_list (list_string : string) : string =
        (* let tabs = String.make level '\t' in *)
        String.concat
          ""
          [ "MalList"; "\n"; "["; list_string; "] eol: "; string_of_token eol ]
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

let string_of_env_map env_data =
  Types.EnvMap.fold
    (fun key value acc ->
      let key_str = Printf.sprintf "%s: %s" key (string_of_mal value) in
      key_str :: acc)
    env_data
    []
  |> String.concat " "
;;

let rec string_of_env (env : Types.env) =
  print_endline "$$$$$$$ printing env";
  match env with
  | { data; outer = None } -> string_of_env_map !data ^ "outer: None"
  | { data; outer = Some outer_env } ->
    string_of_env_map !data ^ "outer: " ^ string_of_env outer_env
;;
