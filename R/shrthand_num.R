#' Process a character vector containing shorthand
#'
#' `shrthnd_num()` coerces a character vector containing numeric data values
#' with non-numeric markers into a numeric vector while also retaining the
#' markers.
#'
#' Data stored in documents and publications are regularly annotated with
#' shorthand and symbols. Often these markers are found in the same container
#' (e.g. a table or spreadsheet cell) as the value they are associated with,
#' which requires further cleaning of the vector to extract the numeric values.
#'
#' A simple approach is to discard the non-numeric components, however these
#' markers convey information which you may wish to retain. `shrthnd_num()`
#' provides a data type that can store both the numeric data and the marker
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
    cli::cli_warn("{.arg x} must be a character vector")
  }

  x <- vctrs::vec_data(x)

  sh_lst <- shrthnd_list(x, shorthand, na_values)

  if (is.null(sh_lst)) {
    cli::cli_warn("no shorthand detected returning {.x} as-is")
    return(x)
  }

  sh_tags <- shrthnd_tags_unique(sh_lst)

  paren_nums <- match.arg(paren_nums)
  base_values <- convert_to_num(x, sh_tags, na_values, paren_nums)

  if (!is.numeric(base_values)) {
    cli::cli_abort("unable to convert {.arg x} to a numeric vector")
  }

  new_shrthnd_num(base_values, sh_lst, digits)

}

new_shrthnd_num <- function(x = numeric(), s = list(), digits = 2L) {

  if (!rlang::is_bare_numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector")
  }

  if (!is_shrthnd_list(s)) {
    cli::cli_abort("{.arg s} must be a shrthnd_list")
  }

  tags <- gen_shrthnd_tags(s, length(x))

  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)

  if (is.integer(x)) {
    vctrs::new_rcrd(list(num = x, tag = tags),
                    class = c("shrthnd_integer", "shrthnd_num"))
  } else {
    vctrs::new_rcrd(list(num = x, tag = tags),
                    digits = digits,
                    class = c("shrthnd_double", "shrthnd_num"))
  }

}

#' @export
vec_ptype_abbr.shrthnd_integer <- function (x, ...) {
  "sh_int"
}

#' @export
vec_ptype_abbr.shrthnd_double <- function (x, ...) {
  "sh_dbl"
}

#' @export
vec_ptype_abbr.shrthnd_num <- function (x, ...) {
  "sh_num"
}

#' @export
format.shrthnd_num <- function(x, digits = TRUE, ...) {
  as_shrthnd(x, digits = digits, .pillar = FALSE)
}

#' @export
pillar_shaft.shrthnd_num <- function(x, ...) {
  out <- as_shrthnd(x, digits = TRUE, .pillar = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @export
vec_ptype2.double.shrthnd_double <- function(x, y, ...) double()

#' @export
vec_cast.double.shrthnd_double <- function(x, to, ...) vctrs::field(x, "num")

#' @export
vec_ptype2.double.shrthnd_integer <- function(x, y, ...) integer()

#' @export
vec_cast.double.shrthnd_integer <- function(x, to, ...) vctrs::field(x, "num")

#' @export
vec_ptype2.integer.shrthnd_integer <- function(x, y, ...) integer()

#' @export
vec_cast.integer.shrthnd_integer <- function(x, to, ...) vctrs::field(x, "num")

#' @export
vec_ptype2.character.shrthnd_double <- function(x, y, ...) character()

#' @export
vec_cast.character.shrthnd_double <- function(x, to, ...) as.character(vctrs::field(x, "num"))

#' @export
vec_ptype2.character.shrthnd_integer <- function(x, y, ...) character()

#' @export
vec_cast.character.shrthnd_integer <- function(x, to, ...) as.character(vctrs::field(x, "num"))
