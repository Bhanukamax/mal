open Types
open Debug_printers

module Env = struct
  let new_env outer_env : env =
    match outer_env with
    | Some _ -> { data = ref EnvMap.empty; outer = outer_env }
    | None -> { data = ref EnvMap.empty; outer = None }
  ;;

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
    | _ -> raise (UNDEFINED_SYMBOL (sym ^ " not found."))
  ;;
end
