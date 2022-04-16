module main
import lex
import os

fn b_read() string {
  i := os.input("user> ")
  return i
}

fn b_eval(i string) string {

  return i
}

fn b_print(i string) {

  println(i)
}

fn main() {

  source := '(+ 123 55 (55 55) 7 89)'
  //source := '(+ 123 55 (- 21 4 ) 77 88)'
  lex.read_str(source)
  //for {
  //b_print(b_eval(b_read()))
  //}

}
