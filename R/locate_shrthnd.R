#' Identify which columns use shrthnd
#'
#' `where_shrthnd_cols()` applies `is_shrthnd_num()` across columns of a
#' data.frame (or elements in a list). `which_shrthnd_cols()` identifies the
#' columns in a data.frame or (elements of a list) by name or index position.
#' `any_shrthnd_cols()` tests whether a data.frame (or list) has any columns
#' that are `shrthnd_num()` vectors.
#'
#' @param x A data.frame (or list)
#' @param .names A logical vector indicating whether to return column names
#'   (the default) or an integer vector of column positions
#'
#' @return For `where_shrthnd_cols()` a logical vector of the same length as
#'   the number columns in `x`. For `which_shrthnd_cols()` a character vector
#'   of names (the default) or an integer vector of index positions for the
#'   columns (or elements) that are shrthnd_num vectors. For
#'   `any_shrthnd_cols()` either `TRUE` if there are any `shrthnd_num()`
#'   vectors in the object or `FALSE` if not.
#'
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' tbl <- tibble::tibble(x = x, sh_x = sh_x)
#'
#' where_shrthnd_cols(tbl)
#'
#' which_shrthnd_cols(tbl)
#'
#' which_shrthnd_cols(tbl, .names = FALSE)
#'
#' any_shrthnd_cols(tbl)
where_shrthnd_cols <- function(x) {

  if (!rlang::is_list(x)) {
    cli::cli_abort("{.arg x} must be a data.frame or list")
  }

  y <- logical(length(x))

  for (i in seq_along(x)) {
    if (is_shrthnd_num(x[[i]])) {
      y[i] <- TRUE
    }
  }

  return(y)

}

#' @rdname where_shrthnd_cols
#' @export
which_shrthnd_cols <- function(x, .names = TRUE) {

  y <- where_shrthnd_cols(x)

  if (!rlang::is_scalar_logical(.names)) {
    cli::cli_abort("{.arg .names} must be a logical vector of length 1")
  }

  if (.names) {

    nms <- names(x)

    if (!is.null(nms) & length(nms) > 0) {
      return(names(x)[y])
    } else {
      cli::cli_alert_warning(
        "{.arg .names} is set to {.val TRUE} but no names in {.arg x}, returning positions"
      )
    }
  }

  return(which(y))

}

#' @rdname where_shrthnd_cols
#' @export
any_shrthnd_cols <- function(x) {
  any(where_shrthnd_cols(x))
}
