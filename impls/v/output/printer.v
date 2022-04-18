module output
import lex
import regex

pub fn pr_str(mut mal lex.MalType) string {

  match mut mal {

    lex.MalList {
      mut str := "("
      for mut item in mal.list {
        str += pr_str(mut &item)
      }
      str += ")"
      query := r'(\s[)])'
      mut re := regex.regex_opt(query) or {panic(err)}
      mut res := re.replace(str, r")")
      //println("ori  :$str, res  :$res")
      return res
    }
    lex.MalAtom {
      return mal.value + " "
    }
  }
  return ""
}
