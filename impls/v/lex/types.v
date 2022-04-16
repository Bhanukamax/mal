module lex
pub type MalType = MalList | MalAtom

pub enum TokenType   {
  number
  symbol
}


pub struct MalList {
  mut:
  list [] MalType

}

pub struct MalAtom  {
  token_type TokenType
  value string
}

