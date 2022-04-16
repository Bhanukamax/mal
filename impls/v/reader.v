
//fn read_str() {
//reader :=  tokenize()
//read_form(reader)
//}

//fn read_form() {

//}

struct Lexer {
  source string
  mut:
    cur_pos int
    tokens []string
    cur_char string
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

fn (mut l Lexer) get_token() {
}


fn tokenize(arg string) {
  source := arg + "\0"
    mut lexer := Lexer{source, 1, [],""}
  lexer.add_token("dd")
  for lexer.peek() != "\0" {
    lexer.get_token()
    lexer.next_char()
  }

  println(lexer)
  println(lexer.tokens)
}


fn main() {
  tokenize("(+ 6 6)")

}
