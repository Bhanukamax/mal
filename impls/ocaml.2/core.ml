open Reader
open Types
open Printer
open Env
open Debug_printers

let get_builtin symbol op env =
  let builtin =
    MalFn
      (fun a ->
        match List.nth_opt a 0, List.nth_opt a 1 with
        | Some (MalAtom (Number x)), Some (MalAtom (Number y)) ->
          if op (int_of_string x) (int_of_string y) then MalAtom True else MalAtom False
        | _ -> MalAtom False)
  in
  let _ = Env.set symbol builtin env in
  env
;;

let list env =
  let _ =
    Env.set
      "list"
      (MalFn (fun a -> MalList { list = a; eol = RParen; listType = List }))
      env
  in
  env
;;

let is_list env =
  let _ =
    Env.set
      "list?"
      (MalFn
         (fun a ->
           match List.nth_opt a 0 with
           | Some (MalList _) -> MalAtom True
           | _ -> MalAtom False))
      env
  in
  env
;;

let count env =
  let _ =
    Env.set
      "count"
      (MalFn
         (fun a ->
           match a with
           | [ MalList list ] ->
             List.length list.list |> fun a -> MalAtom (Number (string_of_int a))
           | _ -> MalAtom (Number "0")))
      env
  in
  env
;;

let is_empty env =
  let _ =
    Env.set
      "empty?"
      (MalFn
         (fun a ->
           match a with
           | [ MalList list ] when List.length list.list == 0 -> MalAtom True
           | _ -> MalAtom False))
      env
  in
  env
;;

let is_equal env =
  let compare_atoms a b =
    match a, b with
    | MalAtom (Number a'), MalAtom (Number b') -> int_of_string a' == int_of_string b'
    | _ -> false
  in
  let equal =
    MalFn
      (fun a ->
        let bool =
          match a with
          | [ MalAtom a; MalAtom b ] -> compare_atoms (MalAtom a) (MalAtom b)
          | [ MalList a; MalList b ] when List.compare_lengths a.list b.list == 0 ->
            List.for_all2 (fun a b -> compare_atoms a b) a.list b.list
          | _ -> false
        in
        match bool with
        | true -> MalAtom True
        | _ -> MalAtom False)
  in
  let _ = Env.set "=" equal env in
  env
;;

let setup_ns env =
  let _ =
    env
    |> get_builtin ">=" ( >= )
    |> get_builtin "<=" ( <= )
    |> get_builtin ">" ( > )
    |> get_builtin "<" ( < )
    (* TODO: write a custom equal function to compare the elements if it's a list *)
    (* |> get_builtin "=" ( == ) *)
    |> list
    |> is_list
    |> count
    |> is_empty
    |> is_equal
  in
  ()
;;
