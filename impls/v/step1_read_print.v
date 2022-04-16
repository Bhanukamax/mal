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

  source := '~@(+ 123 [533 {55 dd what-aname DDD 44} true 4 6] 45 "what a world")'
  lex.read_str(source)
  //for {
  //b_print(b_eval(b_read()))
  //}

}
