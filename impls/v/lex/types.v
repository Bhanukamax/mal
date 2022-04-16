module lex
pub type MalType = MalList | MalAtom

pub enum TokenType {
  number
  symbol
  list
}


pub struct MalList {
  token_type TokenType
 pub mut:
  list [] MalType

}

pub fn new_mal_list() MalList {
  return MalList{TokenType.list, []}
}

pub struct MalAtom  {
  token_type TokenType
  pub:
    value string
}

