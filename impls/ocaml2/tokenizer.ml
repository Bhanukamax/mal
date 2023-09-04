open Types

let acc_char_to_string chars =
  chars |> List.map Char.escaped |> List.rev |> String.concat ""
;;

let is_digit c = Char.(c >= '0' && c <= '9')
let is_alpha c = Char.(c >= 'a' && c <= 'z') || Char.(c >= 'A' && c <= 'Z')

let is_valid_symbol c =
  List.mem c [ ':'; '{'; '}'; '*'; '+'; '-'; '%'; '~'; '`'; '@'; '\\' ]
;;

let should_escape c = List.mem c [ '#'; '|'; '!'; '~'; '^' ]
let is_number_prefix c = Char.(c = '-')

type string_reader =
  { acc : char list
  ; escaped : bool
  }

let get_string_reader acc escaped = { acc; escaped }

let rec read_string (reader : string_reader) (tokens : char list) =
  let acc = reader.acc in
  match tokens with
  (* | '\\' :: '\\' :: rest -> read_string ('\\' :: '\\' :: acc) rest *)
  | '\\' :: '"' :: rest -> read_string (get_string_reader ('"' :: acc) true) rest
  | '"' :: rest -> acc, rest
  | c :: rest -> read_string (get_string_reader (c :: acc) false) rest
  | [] -> raise UN_TERMINATED_STRING_EXCEPTION
;;

let rec read_symbol acc = function
  | c :: rest when is_alpha c || is_digit c || is_valid_symbol c ->
    read_symbol (c :: acc) rest
  | rest -> acc, rest
;;

let rec read_number acc = function
  | c :: rest when is_digit c -> read_number (c :: acc) rest
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
  (* string *)
  | '"' :: rest ->
    let token, rest = read_string (get_string_reader [] false) rest in
    String (acc_char_to_string token) :: tokenize rest
  | sign :: c :: rest when is_number_prefix sign && is_digit c ->
    let num_token, rest = read_number [ c; sign ] rest in
    Number (acc_char_to_string num_token) :: tokenize rest
  | c :: rest when is_digit c ->
    let num_token, rest = read_number [ c ] rest in
    Number (acc_char_to_string num_token) :: tokenize rest
  | c :: rest when should_escape c ->
    let token, rest = read_symbol [ c; '\\'; '\\' ] rest in
    Symbol (token |> acc_char_to_string) :: tokenize rest
  | c :: rest when is_alpha c || is_valid_symbol c ->
    let token, rest = read_symbol [ c ] rest in
    Symbol (token |> acc_char_to_string) :: tokenize rest
  | _ -> []
;;

let tokenize str = str |> String.to_seq |> List.of_seq |> tokenize
