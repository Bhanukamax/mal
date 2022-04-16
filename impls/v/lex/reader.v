module lex
import regex

struct Lexer {
  source string
  mut:
  cur_pos int
  tokens []string
  cur_char string
  cur_source string
}

fn new_lexer(str string) Lexer {
   return Lexer{str, -1, [],"", str}
}

fn (mut l Lexer) add_token(token string) {
  l.tokens << token
}

fn (l Lexer) peek() string {
  return l.tokens[l.cur_pos]
}

fn (mut l Lexer) next() string {
  l.cur_pos++
  return l.tokens[l.cur_pos - 1]
}

pub fn tokenize_regex(source string) []string {
  mut re := regex.regex_opt(r'([\-+()\s])|([0-9])*|(~@)|(".*"?)') or { panic(err)}

  mut lexer := new_lexer(source + "\0 ")

  mut start, mut end := 0, 0

  start, end = re.match_string(lexer.cur_source)
  mut i := 1
  for lexer.cur_source != "" || start != -1 {
    if start == -1 {
      println(" unknown token ${lexer.cur_source[0].ascii_str()}")
      break
    }
    token := lexer.cur_source[start..end]
    lexer.cur_source = lexer.cur_source[end..]
    if token != " " {
      lexer.add_token(token)
    }
    start, end = re.match_string(lexer.cur_source)
    i++
    if i > 1000 || start == -1 {
      println("BEFORE BREAK: start = $start, end = $end, cur_source $lexer.cur_source, len: $lexer.cur_source.len, i > $i")
      break
    }
  }
  return lexer.tokens
}



