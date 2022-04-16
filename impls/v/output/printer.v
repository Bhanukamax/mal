module output
import lex

pub fn pr_str(mut mal lex.MalType) string {

  match mut mal {

    lex.MalList {
      mut str := "("
      for mut item in mal.list {
        str += pr_str(mut &item)
      }
      return str + ")"
    }
    lex.MalAtom {
      return mal.value + " "
    }
  }
  return ""
}
