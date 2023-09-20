open Reader
open Types
open Printer
open Env
open Debug_printers

let setup_ns env =
  Env.set
    "="
    (MalFn
       (fun a ->
         match List.nth_opt a 0, List.nth_opt a 1 with
         | Some (MalAtom (Number x)), Some (MalAtom (Number y)) ->
           if int_of_string x == int_of_string y then MalAtom True else MalAtom False
         | _ -> MalAtom False))
    env
;;
