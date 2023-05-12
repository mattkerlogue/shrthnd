#' shrthnd predicates
#'
#' The `is_shrthnd_*` family of functions test whether a vector is either a
#' `shrthnd_num()`, or a `shrthnd_list()`. `is_shrthnd_integer()` and
#' `is_shrthnd_double()` test whether an object is a `shrthnd_num()` vector
#' and whether the underlying data type is an `integer()` or a `double()`
#'
#' @param x An object to be tested
#'
#' @return A logical vector
#' @export
#' @rdname is_shrthnd
#' @aliases is_shrthnd
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' is_shrthnd_num(sh_x)
#' is_shrthnd_double(sh_x)
#'
#' y <- c("12", "34", "[c]", "NA", "56[e]", "78", "90[e]")
#' sh_y <- shrthnd_num(y, c("[c]", "[e]"))
#' is_shrthnd_num(sh_y)
#' is_shrthnd_integer(sh_y)
#'
#' sh_l <- shrthnd_list(sh_x)
#' is_shrthnd_list(sh_l)
is_shrthnd_num <- function(x) {
  inherits(x, "shrthnd_num")
}

#' @export
#' @rdname is_shrthnd
is_shrthnd_integer <- function(x) {
  cls <- inherits(x, "shrthnd_num")
  if (!cls) {
    return(FALSE)
  }
  typ <- typeof(utils::type.convert(vctrs::field(x, "num"), as.is = TRUE)) == "integer"
  cls && typ
}

#' @export
#' @rdname is_shrthnd
is_shrthnd_double <- function(x) {
  cls <- inherits(x, "shrthnd_num")
  if (!cls) {
    return(FALSE)
  }
  typ <- typeof(utils::type.convert(vctrs::field(x, "num"), as.is = TRUE)) == "double"
  cls && typ
}

#' @export
#' @rdname is_shrthnd
is_shrthnd_list <- function(x) {
  inherits(x, "shrthnd_list")
}
