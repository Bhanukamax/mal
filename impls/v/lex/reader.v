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

pub fn tokenize_regex(source string,mut lexer Lexer) {
  mut re := regex.regex_opt(r'([\-+()\[\]{}\s\0])|([0-9a-zA-Z\-])*|(~@)|(".*"?)') or { panic(err)}


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

    if start == -1 && lexer.cur_source.len > 1 {
      println ("unknown character ${lexer.cur_source[0].ascii_str()}")
      break
    }
    if i > 1000 || start == -1 {
      println("BEFORE BREAK: start = $start, end = $end, cur_source $lexer.cur_source, len: $lexer.cur_source.len, i > $i")
      break
    }
  }
}

pub fn read_str(source string) {

  mut lexer := new_lexer(source + "\0")

  tokenize_regex(source, mut lexer)
  println(lexer.tokens)
}


