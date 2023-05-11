#' Get the tags attached to a shrthnd vector
#'
#' `shrthnd_tags()` provides a character vector the same length as `x` with
#' the shorthand tags, or `NA` if that value has no tag. `shrthnd_tags_unique()`
#' is a convenience wrapper for `unique(shrthnd_tags(x))`, but can also be
#' called on a `shrthnd_list()` object.
#'
#' @param x A `shrthnd_num()` vector
#'
#' @return A character vector
#' @export
#' @rdname shrthnd_tags
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' shrthnd_tags(sh_x)
#' shrthnd_tags_unique(sh_x)
shrthnd_tags <- function(x) {
  if (!is_shrthnd_num(x)) {
    cli::cli_abort("{.arg x} must be a {.cls shrthnd_num}")
  }
  vctrs::field(x, "tag")
}

#' @export
#' @rdname shrthnd_tags
shrthnd_tags_unique <- function(x) {
  if (is_shrthnd_num(x)) {
    tags <- unique(vctrs::field(x, "tag"))
  } else if (is_shrthnd_list(x)) {
    tags <- names(x)
  } else {
    cli::cli_abort("{.arg x} must be either a {.cls shrthnd_num} or {.cls shrthnd_list}")
  }

  out <- tags[(tags != "") & !is.na(tags)]

  return(out)

}

gen_shrthnd_tags <- function(sl, l) {
  out <- character(l)
  sl_s <- names(sl)
  for (i in seq_along(sl)) {
    out[sl[[i]]] <- sl_s[i]
  }
  out[out == ""] <- NA_character_
  return(out)
}
