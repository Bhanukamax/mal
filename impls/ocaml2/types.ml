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
  | UNKNOWN
  | EOF

type malListType =
  | Paren
  | Curly

type mal =
  | MalAtom of token
  | MalList of
      { list : mal list
      ; eol : token
      }

exception UN_TERMINATED_STRING_EXCEPTION
exception ILLEGAL_OPERATION of string
exception UNEXPECTED_STATE of string
