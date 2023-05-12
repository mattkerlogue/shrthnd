digits_combine <- function(x, y) {

  x_digits <- attr(x, "digits")
  y_digits <- attr(y, "digits")

  if (is.null(x_digits) & is.null(y_digits)) {
    return(NULL)
  } else if (!is.null(x_digits)) {
    return(x_digits)
  } else if (!is.null(y_digits)) {
    return(y_digits)
  } else if (x_digits >= y_digits) {
    return(x_digits)
  } else {
    return(y_digits)
  }

}

# self-to-self coercion ---------------------------------------------------

#' @export
vec_ptype2.shrthnd_num.shrthnd_num <- function(x, y, ...) {

  if (identical(attributes(x), attributes(y))) {
    return(x)
  }

  x_nums <- field(x, "num")
  x_tags <- field(x, "tag")
  y_nums <- field(y, "num")
  y_tags <- field(y, "tag")

  digits <- digits_combine(x, y)

  new_nums <- vctrs::vec_c(x_nums, y_nums)
  new_tags <- vctrs::vec_c(x_tags, y_tags)

  if (is.null(digits)) {
    new_shrthnd_num(new_nums, new_tags)
  } else {
    new_shrthnd_num(new_nums, new_tags, digits)
  }

}

#' @export
vec_cast.shrthnd_num.shrthnd_num <- function(x, to, ...) {

  if (identical(attributes(x), attributes(to))) {
    return(x)
  }

  x_nums <- field(x, "num")
  x_tags <- field(x, "tag")
  to_nums <- field(to, "num")
  to_tags <- field(to, "tag")

  digits <- digits_combine(x, to)

  new_nums <- vctrs::vec_c(x_nums, to_nums)
  new_tags <- vctrs::vec_c(x_tags, to_tags)

  if (is.null(digits)) {
    new_shrthnd_num(new_nums, new_tags)
  } else {
    new_shrthnd_num(new_nums, new_tags, digits)
  }

}

# numeric coercion --------------------------------------------------------

# ptype is always a double

#' @export
vec_ptype2.shrthnd_num.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.shrthnd_num <- function(x, y, ...) double()

#' @export
vec_ptype2.shrthnd_num.integer <- function(x, y, ...) double()

#' @export
vec_ptype2.integer.shrthnd_num <- function(x, y, ...) double()

# cast shrthnd_num to numeric

#' @export
vec_cast.double.shrthnd_num <- function(x, to, ...) {
  as.double(vctrs::field(x, "num"))
}

#' @export
vec_cast.integer.shrthnd_num <- function(x, to, ...) {
  if (is_shrthnd_integer(x)) {
    as.integer(vctrs::field(x, "num"))
  } else {
    stop_incompatible_cast(x, to, ...)
  }
}

# can't cast numbers to shrthnd_num

#' @export
vec_cast.shrthnd_num.double <- function(x, to, ...) {
  stop_incompatible_cast(x, to, ...)
}

#' @export
vec_cast.shrthnd_num.integer <- function(x, to, ...) {
  stop_incompatible_cast(x, to, ...)
}

# character coercion ------------------------------------------------------

# ptype is always character

#' @export
vec_ptype2.shrthnd_num.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.shrthnd_num <- function(x, y, ...) character()

# cast to character easily, cast to shrthnd_num via constructor

#' @export
vec_cast.character.shrthnd_num <- function(x, to, ...) {
  as.character(vctrs::field(x, "num"))
}

#' @export
vec_cast.shrthnd_num.character <- function(x, to, ...) {
  shrthnd_num(x)
}
