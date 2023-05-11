#' Process a character vector containing shorthand
#'
#' @param x A character vector of numeric values with shorthand
#' @param shorthand A character vector of shorthand values
#' @param na_values A character vector of NA values
#' @param digits The number of digits for formatting numbers
#'
#' @return A shrthnd_num vector
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' shrthnd_num(x, c("[c]", "[e]"))
shrthnd_num <- function(x, shorthand, na_values = "NA", digits = 2L) {

  if (!rlang::is_character(x)) {
    cli::cli_warn("{.arg x} must be a character vector")
  }

  x <- vctrs::vec_data(x)

  sh_lst <- shrthnd_list(x, shorthand, na_values)

  if (is.null(sh_lst)) {
    cli::cli_warn("returning {.x} as-is")
    return(x)
  }

  base_values <- gsub(shrthnd_regex(shorthand), "", x)
  base_values <- utils::type.convert(base_values, na_values, as.is = TRUE)

  new_shrthnd_num(base_values, sh_lst, digits)

}

new_shrthnd_num <- function(x = numeric(), s = list(), digits = 2L) {

  if (!rlang::is_bare_numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector")
  }

  if (!is_shrthnd_list(s)) {
    cli::cli_abort("{.arg s} must be a shrthnd_list")
  }

  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)

  if (is.integer(x)) {
    vctrs::new_vctr(x, shrthnd = s, class = c("shrthnd_integer", "shrthnd_num"))
  } else {
    vctrs::new_vctr(x, shrthnd = s, digits = digits, class = c("shrthnd_double", "shrthnd_num"))
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
  as_shrthnd(x, digits = digits, .subtle = FALSE)
}

#' @export
pillar_shaft.shrthnd_num <- function(x, ...) {
  out <- as_shrthnd(x, digits = TRUE, .subtle = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @export
vec_ptype2.double.shrthnd_double <- function(x, y, ...) double()

#' @export
vec_cast.double.shrthnd_double <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_ptype2.double.shrthnd_integer <- function(x, y, ...) integer()

#' @export
vec_cast.double.shrthnd_integer <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_ptype2.character.shrthnd_double <- function(x, y, ...) character()

#' @export
vec_cast.character.shrthnd_double <- function(x, to, ...) as.character(vctrs::vec_data(x))

#' @export
vec_ptype2.character.shrthnd_integer <- function(x, y, ...) character()

#' @export
vec_cast.character.shrthnd_integer <- function(x, to, ...) as.character(vctrs::vec_data(x))
