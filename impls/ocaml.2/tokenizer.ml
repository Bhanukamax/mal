open Types

let is_digit c = Char.(c >= '0' && c <= '9')
let is_alpha c = Char.(c >= 'a' && c <= 'z') || Char.(c >= 'A' && c <= 'Z')

let is_valid_symbol c =
  List.mem c [ '!'; ':'; '{'; '}'; '*'; '+'; '-'; '%'; '~'; '`'; '@'; '\\' ]
;;

let should_escape c = List.mem c [ '#'; '|'; '!'; '~'; '^' ]
let is_number_prefix c = Char.(c = '-')

type string_reader =
  { acc : string
  ; escaped : bool
  }

let get_string_reader acc escaped = { acc; escaped }

let rec read_string (reader : string_reader) (tokens : char list) =
  let acc = reader.acc in
  match tokens with
  | '\\' :: '\\' :: rest -> read_string (get_string_reader (acc ^ "\\\\") true) rest
  | '\\' :: '"' :: rest -> read_string (get_string_reader (acc ^ "\\\"") true) rest
  | '\'' :: rest -> read_string (get_string_reader (acc ^ "'") true) rest
  | '\n' :: rest -> read_string (get_string_reader (acc ^ "\n") false) rest
  | '"' :: rest -> acc, rest
  | c :: rest -> read_string (get_string_reader (acc ^ Char.escaped c) false) rest
  | [] -> raise UN_TERMINATED_STRING_EXCEPTION
;;

let rec read_symbol acc = function
  | c :: rest when is_alpha c || is_digit c || is_valid_symbol c ->
    read_symbol (acc ^ Char.escaped c) rest
  | rest -> acc, rest
;;

let rec read_keyword acc = function
  | c :: rest when is_alpha c || is_digit c || is_valid_symbol c ->
    read_symbol (acc ^ Char.escaped c) rest
  | rest -> acc, rest
;;

let string_of_char_list list = List.map Char.escaped list |> String.concat ""

let rec read_number acc = function
  | c :: rest when is_digit c -> read_number (acc ^ Char.escaped c) rest
  | rest -> acc, rest
;;

let rec tokenize (chars : char list) : token list =
  match chars with
  | ' ' :: rest -> tokenize rest
  | ',' :: rest -> tokenize rest
  | '(' :: rest -> LParen :: tokenize rest
  | ')' :: rest -> RParen :: tokenize rest
  | '{' :: rest -> LCurly :: tokenize rest
  | '}' :: rest -> RCurly :: tokenize rest
  | '[' :: rest -> LBracket :: tokenize rest
  | ']' :: rest -> RBracket :: tokenize rest
  | '\'' :: rest -> Symbol "\'" :: tokenize rest
  | '`' :: rest -> Symbol "`" :: tokenize rest
  | '~' :: '@' :: rest -> Symbol "~@" :: tokenize rest
  | '-' :: '>' :: '>' :: rest -> Symbol "->>" :: tokenize rest
  | '@' :: rest -> Symbol "@" :: tokenize rest
  | '~' :: rest -> Symbol "~" :: tokenize rest
  | '*' :: '*' :: rest -> Symbol "**" :: tokenize rest
  | '*' :: rest -> Symbol "*" :: tokenize rest
  | '/' :: rest -> Symbol "/" :: tokenize rest
  (* string *)
  | ':' :: rest ->
    let token, rest = read_keyword ":" rest in
    Keyword token :: tokenize rest
  | '"' :: rest ->
    let token, rest = read_string (get_string_reader "" false) rest in
    String token :: tokenize rest
  | sign :: c :: rest when is_number_prefix sign && is_digit c ->
    let num_token, rest = read_number (string_of_char_list [ sign; c ]) rest in
    Number num_token :: tokenize rest
  | c :: rest when is_digit c ->
    let num_token, rest = read_number (Char.escaped c) rest in
    Number num_token :: tokenize rest
  | c :: rest when is_alpha c || is_valid_symbol c ->
    let token, rest = read_symbol (Char.escaped c) rest in
    Symbol token :: tokenize rest
  | _ -> []
;;

let tokenize str = str |> String.to_seq |> List.of_seq |> tokenize
