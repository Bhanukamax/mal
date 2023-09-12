open Types

module Env = struct
  module EnvMap = Map.Make (String)

  type env =
    { data : mal EnvMap.t
    ; outer : env option
    }

  let new_env : env = { data = EnvMap.empty; outer = None }
  let set sym mal env : env = { env with data = EnvMap.add sym mal env.data }

  let rec find_opt sym env =
    let found = EnvMap.find_opt sym env.data in
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
