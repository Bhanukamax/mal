open Types
open Debug_printers

module Env = struct
  let set sym mal env =
    let _ = env.data := EnvMap.add sym mal !(env.data) in
    env
  ;;

  let rec set_env_bind_list binds exprs env =
    let mapped_env = List.mapi (fun i b -> set b (List.nth exprs i) env) binds in
    match mapped_env |> List.rev |> fun list -> List.nth_opt list 0 with
    | Some env -> env
    | None -> env
  ;;

  let repl_env = { data = ref EnvMap.empty; outer = None }

  let new_env outer_env binds exprs : env =
    { data = ref EnvMap.empty; outer = outer_env } |> set_env_bind_list binds exprs
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
