open Reader
open Types
open Printer
open Env
open Debug_printers

let get_builtin symbol op env =
  let _ =
    Env.set
      symbol
      (MalFn
         (fun a ->
           match List.nth_opt a 0, List.nth_opt a 1 with
           | Some (MalAtom (Number x)), Some (MalAtom (Number y)) ->
             if op (int_of_string x) (int_of_string y)
             then MalAtom True
             else MalAtom False
           | _ -> MalAtom False))
      env
  in
  ()
;;

let setup_ns env =
  get_builtin ">=" ( >= ) env;
  get_builtin "<=" ( <= ) env;
  get_builtin ">" ( > ) env;
  get_builtin "<" ( < ) env;
  get_builtin "=" ( == ) env
;;
