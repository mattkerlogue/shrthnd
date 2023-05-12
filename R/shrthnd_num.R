#' Convert a character vector containing shorthand
#'
#' `shrthnd_num()` coerces a character vector containing numeric data values
#' with non-numeric tags into a numeric-like vector while also retaining the
#' tags.
#'
#' Data stored in documents and publications are regularly annotated with
#' shorthand and symbols. Often these tags are found in the same container
#' (e.g. a table or spreadsheet cell) as the value they are associated with,
#' which requires further cleaning of the vector to extract the numeric values.
#'
#' A simple approach is to discard the non-numeric components, however these
#' tags can convey information which you may wish to retain. `shrthnd_num()`
#' provides a data type that can store both the numeric data and the marker.
#'
#' By default `shrthnd_num()` will extract any non-numeric values following
#' numeric ones and process them as a shorthand tag. However, you can
#' optionally supply a vector of tags, using the `shorthand` argument, if you
#' wish to validate the extracted tags and only accept vectors with specific
#' shorthand values.
#'
#' If the underlying numeric values are real numbers (i.e. a `double()` vector)
#' the `digits` argument will be used to format the display of the
#' `shrthnd_dbl` vector (defaults to 2 decimal places).
#'
#' @param x A character vector of numeric values with shorthand
#' @param shorthand A character vector of shorthand values
#' @param na_values A character vector of NA values
#' @param digits The number of digits for formatting numbers
#' @param paren_nums How to handle numbers in parenthesis (e.g. `(12,435.43)`),
#'   defaults to negative as most commonly used in accounting to denote
#'   negative values instead of a minus symbol preceding the value
#'
#' @return A shrthnd_num vector
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' shrthnd_num(x, c("[c]", "[e]"))
shrthnd_num <- function(x, shorthand = NULL, na_values = "NA", digits = 2L,
                        paren_nums = c("negative", "strip")) {

  if (!rlang::is_character(x)) {
    cli::cli_abort(
      "{.arg x} must be a character vector"
    )
  }

  x <- vctrs::vec_data(x)
  x <- strip_percent(x)

  sh_lst <- shrthnd_list(x, shorthand, na_values)

  if (is.null(sh_lst)) {
    cli::cli_abort("unable to convert {.arg x} to a {.cls shrthnd_num}")
  }

  tags <- gen_shrthnd_tags(sh_lst, length(x))
  unq_tags <- shrthnd_unique_tags(sh_lst)

  paren_nums <- match.arg(paren_nums)
  base_values <- convert_to_num(x, unq_tags, na_values, paren_nums)

  if (!is.numeric(base_values)) {
    cli::cli_abort("unable to convert {.arg x} to a numeric vector")
  }

  new_shrthnd_num(base_values, tags, digits)

}

#' Make a shrthnd_num vector from numeric and character components
#'
#' `make_shrthnd_num()` allows you to construct a `shrthnd_num` vector from
#' a numeric vector of data values and a character vector of shorthand markers.
#'
#' @param x A numeric vector
#' @param tags A character vector
#' @param digits The number of digits to format the numeric vector with
#'
#' @return A `shrthnd_num` vector
#' @export
#'
#' @examples
#' make_shrthnd_num(c(1:3, NA, 4:5, NA), c("", "", "", "[c]", "", "[e]", NA))
make_shrthnd_num <- function(x = numeric(), tags = character(), digits = 2L) {

  x <- utils::type.convert(x, as.is = TRUE)

  tags <- vctrs::vec_cast(tags, character())
  tags <- gsub("^(\\s)", "", tags)
  tags <- gsub("(\\s)$", "", tags)

  tags[tags == ""] <- NA_character_

  new_shrthnd_num(x, tags, digits)

}

new_shrthnd_num <- function(x = numeric(), tags = character(), digits = 2L) {

  if (!rlang::is_bare_numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector")
  }

  if (!rlang::is_bare_character(tags)) {
    cli::cli_abort("{.arg tags} must be a character vector")
  }

  if (length(x) != length(tags)) {
    cli::cli_abort("{.arg x} and {.arg tags} must be the same length")
  }

  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)

  if (is.integer(x)) {
    vctrs::new_rcrd(list(num = x, tag = tags), class = "shrthnd_num")
  } else {
    vctrs::new_rcrd(list(num = x, tag = tags), digits = digits,
                    class = "shrthnd_num")
  }

}

#' @export
vec_ptype_abbr.shrthnd_num <- function (x, ...) {
  if (is_shrthnd_integer(x)) {
    "sh_int"
  } else {
    "sh_dbl"
  }
}

#' @export
format.shrthnd_num <- function(x, ...) {
  as_shrthnd(x, .pillar = FALSE, ...)
}

#' @export
pillar_shaft.shrthnd_num <- function(x, ...) {
  out <- as_shrthnd(x, .pillar = TRUE, ...)
  pillar::new_pillar_shaft_simple(out, align = "right")
}
