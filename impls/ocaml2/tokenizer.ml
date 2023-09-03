open Types

let acc_char_to_string chars =
  chars |> List.map Char.escaped |> List.rev |> String.concat ""
;;

let is_digit c = Char.(c >= '0' && c <= '9')
let is_alpha c = Char.(c >= 'a' && c <= 'z') || Char.(c >= 'A' && c <= 'Z')
let is_valid_symbol c = Char.(c = '-' || c = '%' || c = '@')
let is_number_prefix c = Char.(c = '-')

let rec read_string acc = function
  | '"' :: rest -> acc, rest
  | c :: rest -> read_string (c :: acc) rest
  | rest -> raise UN_TERMINATED_STRING_EXCEPTION
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
  | '(' :: rest -> LParen :: tokenize rest
  | ')' :: rest -> RParen :: tokenize rest
  | '"' :: rest ->
    let token, rest = read_string [] rest in
    String (acc_char_to_string token) :: tokenize rest
  | c :: rest when is_alpha c ->
    let token, rest = read_symbol [ c ] rest in
    Symbol (token |> acc_char_to_string) :: tokenize rest
  | sign :: c :: rest when is_number_prefix sign && is_digit c ->
    let num_token, rest = read_number [  c; sign ] rest in
    Number (acc_char_to_string num_token) :: tokenize rest
  | c :: rest when is_digit c ->
    let num_token, rest = read_number [ c ] rest in
    Number (acc_char_to_string num_token) :: tokenize rest
  | _ :: rest -> UNKNOWN :: tokenize rest
  | _ -> []
;;

let tokenize str = str |> String.to_seq |> List.of_seq |> tokenize
