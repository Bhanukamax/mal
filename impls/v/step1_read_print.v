module main
import lex
import output
import os

fn b_read() lex.MalList {
  i := os.input("user> ")
  return lex.read_str(i)
}

fn b_eval(i lex.MalList) lex.MalList {

  return i
}

fn b_print(mut i lex.MalList) {
  mut str := ""
  for mut item in i.list {
    str += output.pr_str(mut item)
  }
  println(str)
}

fn main() {

  //source := '(+ 123 55 (- 55 55 (- 22 45)) 7 89)'
  //source := '(+ 123 55 (- 21 4 ) 77 88)'
  //mut mal := lex.read_str(source)
  //println(mal)
  //str:=output.pr_str(mut mal)



  //println(str)
  for {
  mut ast := b_read()
  mut eval := b_eval(ast)
  b_print(mut eval)
  }

}
