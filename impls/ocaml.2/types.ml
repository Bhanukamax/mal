type token =
  | LParen
  | RParen
  | LCurly
  | RCurly
  | LBracket
  | RBracket
  | Number of string
  | String of string
  | Symbol of string
  | Keyword of string
  | UNKNOWN
  | EOF

type malListType =
  | Paren
  | Curly

type listType =
  | List
  | HashMap

type mal =
  | MalAtom of token
  | MalFn of (mal list -> mal)
  | MalList of
      { list : mal list
      ; eol : token
      ; listType : listType
      }

module Mal = struct
  type t = mal
end

exception UN_TERMINATED_STRING_EXCEPTION
exception ILLEGAL_OPERATION of string
exception UNEXPECTED_STATE of string
