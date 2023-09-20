open Types
open Debug_printers

module Env = struct
  let set sym mal env =
    let _ = env.data := EnvMap.add sym mal !(env.data) in
    env
  ;;

  (* TODO:
    bind the binds to env
    *)
  let rec set_env_bind_list binds exprs env = env
  let repl_env = { data = ref EnvMap.empty; outer = None }

  let new_env outer_env binds exprs : env =
    match outer_env with
    | Some _ -> { data = ref EnvMap.empty; outer = outer_env }
    | None ->
      { data = ref EnvMap.empty; outer = None }
      (* ************** just to break line ***********  *)
      |> set_env_bind_list binds exprs
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
