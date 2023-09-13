open Types
open Debug_printers

module Env = struct
  module EnvMap = Map.Make (String)

  type env =
    { data : mal EnvMap.t ref
    ; outer : env option
    }

  let new_env : env = { data = ref EnvMap.empty; outer = None }

  let set sym mal env =
    let _ = env.data := EnvMap.add sym mal !(env.data) in
    env
  ;;

  let rec find_opt sym env =
    let found = EnvMap.find_opt sym !(env.data) in
    match found, env.outer with
    | Some f, _ -> Some f
    | None, Some outer_env -> find_opt sym outer_env
    | None, None -> None
  ;;

  let get sym env =
    let found = find_opt sym env in
    match found with
    | Some f -> f
    | _ -> raise (ILLEGAL_OPERATION "undefined symbol")
  ;;
end

let string_of_env_map env_data =
  Env.EnvMap.fold
    (fun key value acc ->
      let key_str = Printf.sprintf "%s: %s" key (string_of_mal value) in
      key_str :: acc)
    env_data
    []
  |> String.concat " "
;;

let rec string_of_env (env : Env.env) =
  match env with
  | { data; outer = None } -> string_of_env_map !data ^ "outer: None"
  | { data; outer = Some outer_env } ->
    string_of_env_map !data ^ "outer: " ^ string_of_env outer_env
;;
