shrthnd_regex <- function(string) {
  string <- string[!(string == "")]
  string[string == "."] <- "^.$"
  string <- stringr::str_replace_all(string, "([.\\\\|*+?{}\\[\\]()])", "\\\\\\1")
  paste0(string, collapse = "|")
}

extract_text <- function(x, na_values = "NA") {

  out <- gsub("(^[-\\(]?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?\\)?)(.*$)", "\\5", x)
  out <- gsub(shrthnd_regex(na_values), "", out)
  out <- gsub("^(\\s)", "", out)
  out <- gsub("(\\s)$", "", out)

  return(out)

}

extract_num <- function(x, paren_nums = c("negative", "strip")) {

  paren_nums <- match.arg(paren_nums)

  paren_replace <- "\\1"

  if (paren_nums == "negative") {
    paren_replace <- paste0("-", paren_replace)
  }

  out <- gsub("(?:^\\((\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?)\\))(.*$)", paren_replace, x)

  out <- gsub(",", "", out)

  out <- gsub("^(\\s)", "", out)
  out <- gsub("(\\s)$", "", out)

  return(out)

}

convert_to_num <- function(x, shorthand, na_values = "NA", paren_nums) {
  x <- gsub(shrthnd_regex(shorthand), "", x)
  x <- gsub("(^[-\\(]?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?\\)?)(.*$)", "\\1", x)
  x <- extract_num(x, paren_nums)
  utils::type.convert(x, na_values, as.is = TRUE)
}

validate_tags <- function(x, shorthand) {

  if (sum(!(x %in% shorthand)) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}

where_tags <- function(tags, unique_tags) {

  where <- purrr::map(
    unique_tags, ~which(tags == .x, useNames = FALSE)
  )

  return(where)

}

strip_percent <- function(x) {
  x <- gsub("%", "", x)
}
