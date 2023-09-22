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
    | MalAtom (String a'), MalAtom (String b') -> String.compare a' b' == 0
    | _ -> false
  in
  let rec eq a =
    match a with
    | [ MalAtom Nil; MalAtom Nil ] -> true
    | [ MalAtom True; MalAtom True ] -> true
    | [ MalAtom False; MalAtom True ] -> false
    | [ MalAtom True; MalAtom False ] -> false
    | [ MalAtom False; MalAtom False ] -> true
    | [ MalAtom a; MalAtom b ] -> compare_atoms (MalAtom a) (MalAtom b)
    | [ MalList a; MalList b ] when List.compare_lengths a.list b.list == 0 ->
      List.for_all2 (fun a b -> eq [ a; b ]) a.list b.list
    | _ -> false
  in
  let equal =
    MalFn
      (fun args ->
        match eq args with
        | true -> MalAtom True
        | _ -> MalAtom False)
  in
  let _ = Env.set "=" equal env in
  env
;;

let prn env =
  let _ =
    Env.set
      "prn"
      (MalFn
         (fun a ->
           match a with
           | [ MalAtom (Number a) ] | [ MalAtom (Symbol a) ] ->
             print_endline a;
             MalAtom Nil
           | _ ->
             print_endline "";
             MalAtom Nil))
      env
  in
  env
;;

let pr_str env =
  let _ =
    Env.set
      "pr-str"
      (MalFn
         (fun a ->
           let list = List.map (fun a' -> Printer.pr_str a') a in
           MalAtom (String (String.concat "" list))))
      env
  in
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
    |> prn
    |> pr_str
  in
  ()
;;
