# build a regex pattern, based on stringr::str_escape()
shrthnd_regex <- function(string) {
  string <- string[!(string == "")]
  string[string == "."] <- "^.$"
  string <- stringr::str_replace_all(string, "([.\\\\|*+?{}\\[\\]()])", "\\\\\\1")
  paste0(string, collapse = "|")
}

strip_num <- function(x, paren_nums = c("negative", "strip")) {

  paren_nums <- match.arg(paren_nums)

  paren_replace <- "\\1"

  if (paren_nums == "negative") {
    paren_replace <- paste0("-", paren_replace)
  }

  out <- gsub("(?:^\\((\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?)\\))(.*$)", paren_replace, x)

  out <- gsub(",", "", out)

  return(out)

}

convert_to_num <- function(x, shorthand, na_values = "NA", paren_nums) {
  x <- gsub(shrthnd_regex(shorthand), "", x)
  x <- gsub("(^[-\\(]?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?\\)?)(.*$)", "\\1", x)
  x <- strip_num(x, paren_nums)
  utils::type.convert(x, na_values, as.is = TRUE)
}
