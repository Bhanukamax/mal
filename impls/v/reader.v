import regex
//fn read_str() {
//reader :=  tokenize()
//read_form(reader)
//}

//fn read_form() {

//}

enum TokenType {
  lparen
  rparen
  plus
  number
}

struct Token {
  token_type TokenType
  token string
}

struct Lexer {
  source string
  mut:
    cur_pos int
    tokens []Token
    cur_char string
}

fn (mut l Lexer) add_token(token Token) {
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

fn (mut l Lexer) add_cur_char_token(token_type TokenType) {
  l.add_token(Token{token_type, l.cur_char})
}

fn (mut l Lexer) get_token() {
  if l.cur_char == "(" {
    l.add_cur_char_token(TokenType.lparen)
    return
  }
  if l.cur_char == ")" {
    l.add_cur_char_token(TokenType.rparen)
    return
  }
  if l.cur_char == "+" {
    l.add_cur_char_token(TokenType.plus)
    return
  }

  // Handle numbers
  mut re := regex.regex_opt(r'[0-9]') or { panic(err)}
  mut _, mut end := re.match_string(l.cur_char)
  if end != 0 {
    mut cur_token := l.cur_char
    _, end = re.match_string(l.peek())
    for end != 0  {
      l.next_char()
      cur_token += l.cur_char
      _, end = re.match_string(l.peek())
    }
    l.add_token(Token{TokenType.number, cur_token})
    return
  }

  if l.cur_char == " " { return }

}


fn tokenize(arg string) {
  source := arg + "\0"
  mut lexer := Lexer{source, -1, [],""}
  println("source >>> $lexer.source")
  for lexer.peek() != "\0" {
    lexer.next_char()
    lexer.get_token()
  }
  lexer.next_char()
  println(lexer.tokens)
}


fn main() {
  tokenize("(+ 123 67)")
  tokenize("(+ 123 7)")
}
