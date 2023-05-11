#' shrthnd predicates
#'
#' The `is_shrthnd_*` family of functions test whether a vector is either a
#' `shrthnd_num()`, `shrthnd_integer()`, `shrthnd_double()` or a `shrthnd_list()`
#'
#' @param x A shrthnd object
#'
#' @return A logical vector
#' @export
#' @rdname is_shrthnd
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
  inherits(x, "shrthnd_integer")
}

#' @export
#' @rdname is_shrthnd
is_shrthnd_double <- function(x) {
  inherits(x, "shrthnd_double")
}

#' @export
#' @rdname is_shrthnd
is_shrthnd_list <- function(x) {
  inherits(x, "shrthnd_list")
}
