#' Arithmetic and mathematical operations
#'
#' Arithmetic and most mathematical operations are supported on the numeric
#' component of `shrthnd_num()` vectors via the `{vctrs}` package without
#' having to wrap the vector in `as.numeric()`.
#'
#' You can use all the standard arithmetic infix operators (`+`, `-`, `/`, `*`,
#' `^`, `%%`, `%/%`, `!`). See `vctrs::vec_arith()` for further details.
#'
#' Through `vctrs::vec_math()` the following generic mathematical operations
#' are supported:
#'
#' - from the [Summary] group generic:
#'   `prod()`, `sum()`, `any()`, `all()`.
#'
#' - from the [Math] group generic:
#'   `abs()`, `sign()`, `sqrt()`, `ceiling()`, `floor()`, `trunc()`, `cummax()`,
#'   `cummin()`, `cumprod()`, `cumsum()`, `log()`, `log10()`, `log2()`,
#'   `log1p()`, `acos()`, `acosh()`, `asin()`, `asinh()`, `atan()`, `atanh()`,
#'   `exp()`, `expm1()`, `cos()`, `cosh()`, `cospi()`, `sin()`, `sinh()`,
#'   `sinpi()`, `tan()`, `tanh()`, `tanpi()`, `gamma()`, `lgamma()`,
#'   `digamma()`, `trigamma()`.
#'
#' - `vectrs::vec_math()` also enables support for `mean()`, `is.nan()`,
#'   `is.finite()` and `is.infinite()`.
#'
#' - In addition to these, the `{shrthnd}` package also provides methods for
#'   `median()` and `quantile()`, and will work with `sd()` due to the ability
#'   of a `shrthnd_num()` to be easily coerced to a numeric vector.
#'
#' For other operations you will need to wrap the `shrthnd_num` vector in
#' `as.numeric()`.
#'
#' For all operations remember that you will likely need to set `na.rm = TRUE`
#' or whatever other method a function has for ignoring missing values.
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#'
#' sh_x * 2
#'
#' 2 + sh_x
#'
#' mean(sh_x, na.rm = TRUE)
#'
#' @name shrthnd_maths
NULL

shrthnd_arith <- function(op, x, y, ...) {
  x <- vctrs::vec_cast(x, double())
  y <- vctrs::vec_cast(y, double())
  vctrs::vec_arith_base(op, x, y)
}

shrthnd_math <- function(.fn, .x, ...) {
  .x <- vctrs::vec_cast(.x, double())
  vctrs::vec_math_base(.fn, .x, ...)
}

#' @export
#' @method vec_arith shrthnd_num
vec_arith.shrthnd_num <- function(op, x, y, ...) {
  UseMethod("vec_arith.shrthnd_num", y)
}

#' @export
#' @method vec_arith.shrthnd_num numeric
vec_arith.shrthnd_num.numeric <- function(op, x, y, ...) {
  shrthnd_arith(op, x, y, ...)
}

#' @export
#' @method vec_arith.numeric shrthnd_num
vec_arith.numeric.shrthnd_num <- function(op, x, y, ...) {
  shrthnd_arith(op, x, y, ...)
}

#' @export
#' @method vec_arith.shrthnd_num shrthnd_num
vec_arith.shrthnd_num.shrthnd_num <- function(op, x, y, ...) {
  shrthnd_arith(op, x, y, ...)
}

#' @export
vec_math.shrthnd_num <- function(.fn, .x, ...) {
  shrthnd_math(.fn, .x, ...)
}

#' @importFrom stats median
#' @export
median.shrthnd_num <- function(x, ...) {
  x <- vctrs::vec_cast(x, double())
  median(x, ...)
}

#' @importFrom stats quantile
#' @export
quantile.shrthnd_num <- function(x, ...) {
  x <- vctrs::vec_cast(x, double())
  quantile(x, ...)
}
