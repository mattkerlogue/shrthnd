# helper functions for constructing shrthnd_num

# generate a concatenated regex string with special characters escaped.
# special character escaping is based on stringr::str_escape() but modified
#   to avoid removing decimal points, single dot is only included if the
#   full content.
# e.g. shrthnd_regex(c("[c]", "[e]", ".", "*")) becomes:
#   "\\[c\\]|\\[e\\]|^.$|\\*"
shrthnd_regex <- function(string) {
  string <- string[!(string == "")]
  string[string == "."] <- "^.$"
  string <- stringr::str_replace_all(string, "([.\\\\|*+?{}\\[\\]()])", "\\\\\\1")
  paste0(string, collapse = "|")
}

# get the non-numeric component of a vector, trimmed for spaces
extract_text <- function(x, na_values = c("", "NA"),
                         dec = ".", bigmark = ",") {

  nr <- num_regex(dec, bigmark)

  out <- gsub(nr, "\\5", x)
  out <- gsub(shrthnd_regex(na_values), "", out)
  out <- gsub("^(\\s)", "", out)
  out <- gsub("(\\s)$", "", out)

  return(out)

}

# extract the numeric component of a vector
# accounting formats commonly put parentheses around numbers
extract_num <- function(x, paren_nums = c("negative", "strip"),
                        na_values = c("", "NA"), dec = ".", bigmark = ",") {

  paren_nums <- match.arg(paren_nums)
  paren_replace <- "\\1"

  if (paren_nums == "negative") {
    paren_replace <- paste0("-", paren_replace)
  }

  # strip leading space
  x <- gsub("^(\\s)", "", x)

  # escape dec/bigmark and get regex pattern
  nr <- num_regex(dec, bigmark)

  # extract numeric component
  num <- gsub(nr, "\\1", x)
  num <- gsub(bigmark, "", num)
  num <- gsub("^\\(", paren_replace, num)
  num <- gsub("\\)$", "", num)
  num <- gsub("^\\+", "", num)
  num <- gsub("(\\s)$", "", num)

  out <- utils::type.convert(num, na_values, as.is = TRUE)

  return(out)

}

# convert a character vector into a numeric vector
convert_to_num <- function(x, shorthand, na_values = c("", "NA"),
                           paren_nums, dec = ".", bigmark = ",") {
  x <- gsub(shrthnd_regex(shorthand), "", x)
  x <- extract_num(x, paren_nums, na_values, dec, bigmark = ",")
  return(x)
}

# given a vector of tags check if they are in a vector of shorthand symbols
validate_tags <- function(x, shorthand) {

  if (sum(!(x %in% shorthand)) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}

# get tag locations
find_tag_locations <- function(tags, unique_tags) {

  where <- purrr::map(
    unique_tags, ~which(tags == .x, useNames = FALSE)
  )

  return(where)

}

# remove percentage symbol
strip_percent <- function(x) {
  x <- gsub("%", "", x)
}

# regex for number detection
# based on "(^[-+\\(]?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?\\)?)(.*)"
num_regex <- function(dec, bigmark) {

  check_dec_bigmark(dec, bigmark)

  bigmark <- stringr::str_escape(bigmark)
  dec <- stringr::str_escape(dec)

  paste0("(^[-+\\(]?\\d+(", bigmark, "\\d+)*(", dec, "\\d+(e\\d+)?)?\\)?)(.*)")
}


check_dec_bigmark <- function(dec, bigmark) {

  if (!rlang::is_scalar_character(dec)) {
    cli::cli_abort("{.arg dec} must be a character vector of length 1")
  }

  if (nchar(dec) != 1) {
    cli::cli_abort("{.arg dec} must be a single character")
  }

  if (!rlang::is_scalar_character(bigmark)) {
    cli::cli_abort("{.arg bigmark} must be a character vector of length 1")
  }

  if (nchar(bigmark) != 1) {
    cli::cli_abort("{.arg bigmark} must be a single character")
  }

  if (dec == bigmark) {
    cli::cli_abort("{.arg dec} and {.arg bigmark} must be different")
  }

}


chk_arg <- function(x, type = c("character", "integer", "double"),
                    scalar = FALSE, bare = TRUE, allow_null = FALSE) {

  if (allow_null) {
    if (is.null(x)) {
      return(TRUE)
    }
  } else if (is.null(x)) {
    return(FALSE)
  }

  type <- rlang::arg_match(type)

  if (scalar) {
    type <- paste0("scalar_", type)
  } else if (bare) {
    type <- paste0("bare_", type)
  }

  switch(
    type,
    scalar_character = rlang::is_scalar_character(x),
    scalar_integer = rlang::is_scalar_integerish(x),
    scalar_double = rlang::is_scalar_double(x),
    bare_character = rlang::is_bare_character(x),
    bare_integer = rlang::is_bare_integer(x),
    bare_double = rlang::is_bare_double(x),
    character = rlang::is_character(x),
    integer = rlang::is_integer(x),
    double = rlang::is_double(x)
  )

}
