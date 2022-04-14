import os

fn b_read() string{
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

  for {
    b_print(b_eval(b_read()))
  }

}
