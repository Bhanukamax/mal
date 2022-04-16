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
  return Lexer{str, 0, [],"", str}
}

fn (mut l Lexer) add_token(token string) {
  l.tokens << token
}

fn (l Lexer) peek() string {
  return l.tokens[l.cur_pos]
}

fn (mut l Lexer) next() string {
  if l.peek() == "EOF" {
    return l.peek()
  }
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
  lexer.add_token("EOF")
}

fn read_form(mut lexer &Lexer) MalType {

  //lexer.next()
  match lexer.peek() {
    "(" {
      //lexer.next()
      return read_list(mut lexer)
    }
    else {
      return read_atom(mut lexer)
    }
  }
}

fn read_list(mut lexer &Lexer) MalType {
  mut list := MalList {[]}
  for lexer.peek() != ")" {
    lexer.next()
    mut atom := read_form(mut &lexer)
    match mut atom {
      MalList {
        println(">>>>LIST $atom")
        list.list << atom
      }
      MalAtom {
        match atom.value {
          ")" { }
          "EOF" {
            panic ("bad EOF")
          }
          else {
            list.list << atom
          }
        }
      }
    }
  }
  //lexer.next()

  return list
}

fn read_atom(mut lexer &Lexer) MalType {


  mut re := regex.regex_opt(r'[0-9]*') or { panic(err)}
  start, end := re.match_string(lexer.peek())
  if start > -1 {
  return MalAtom{TokenType.number, lexer.peek()}
  }


  return MalAtom{TokenType.symbol, lexer.peek()}
}

pub fn read_str(source string) {
  mut lexer := new_lexer(source + "\0")


  tokenize_regex(source, mut lexer)
  mut list := [] MalType{}
  for lexer.peek() != "EOF" {
    //list << read_form(mut &lexer)
    //list << read_form(mut &lexer)

    mut atom := read_form(mut &lexer)
    match mut atom {
      MalList {
        println(">>>>LIST $atom")
        list << atom
      }
      MalAtom {
        match atom.value {
          ")" {
          }
          "EOF" {
            panic ("bad EOF")
          }
          else {
            list << atom
          }
        }
      }
    }

    lexer.next()
  }

  println(lexer.tokens)
  println(list)
}


