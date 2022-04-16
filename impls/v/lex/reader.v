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

fn (mut l Lexer) add_token(token string) {
  l.tokens << token
}

fn (mut l Lexer) peek() string {
  if l.cur_pos + 1 >= l.source.len {
    return "\0"
  }

  return l.source[l.cur_pos + 1].ascii_str()
}

fn (mut l Lexer)  next_char() {
  l.cur_pos++
  if l.cur_pos >= l.source.len {
    // # EOF
    l.cur_char = "\0"
  } else {
    l.cur_char = l.source[l.cur_pos].ascii_str()
  }
}

fn (mut l Lexer) add_cur_char_token() {
  l.add_token(l.cur_char)
}

fn (mut l Lexer) get_token() {
  mut re := regex.regex_opt(r'[\-+()]') or { panic(err)}
  mut _, mut end := re.match_string(l.cur_char)
  if end != 0 {
    l.add_cur_char_token()
    return
  }


  // Handle numbers
  re = regex.regex_opt(r'[0-9]') or { panic(err)}
  _, end = re.match_string(l.cur_char)
  if end != 0 {
    mut cur_token := l.cur_char
    _, end = re.match_string(l.peek())
    for end != 0  {
      l.next_char()
      cur_token += l.cur_char
      _, end = re.match_string(l.peek())
    }
    l.add_token(cur_token)
    return
  }

  if l.cur_char == " " { return }
}


fn read_str() {

}

pub fn tokenize_regex(arg string) []string {
  source := arg + "\0 "
  mut re := regex.regex_opt(r'([\-+()\s])|([0-9])*|(~@)') or { panic(err)}
  // mut re := regex.regex_opt(r'[0-9]*') or { panic(err)}
  mut lexer := Lexer{source, -1, [],"", source}
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

pub fn tokenize(arg string) []string{
  source := arg + "\0"
  mut lexer := Lexer{source, -1, [],"", ""}
  println("source >>> $lexer.source")
  for lexer.peek() != "\0" {
    lexer.next_char()
    lexer.get_token()
  }
  lexer.next_char()
  return lexer.tokens
}


