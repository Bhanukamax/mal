module util

pub fn char_eq_any(str string, chars [] string) bool {
  for c in chars {
    if str == c { return true }
  }

  return false
}
