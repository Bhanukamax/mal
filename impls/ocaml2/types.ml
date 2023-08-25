type token =
  | LParen
  | RParen
  | Number of string
  | String of string
  | Symbol of string
  | UNKNOWN
  | EOF

type mal =
  | MalAtom of token
  | MalList of mal list

exception UN_TERMINATED_STRING_EXCEPTION
exception ILLEGAL_OPERATION of string
exception UNEXPECTED_STATE of string
