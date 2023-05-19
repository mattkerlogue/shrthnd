#' Get the tags attached to a shrthnd vector
#'
#' `shrthnd_tags()` provides a character vector the same length as `x` with
#' the shorthand tags, or `NA` if that value has no tag. `shrthnd_unique_tags()`
#' is a convenience wrapper for `unique(shrthnd_tags(x))`, but can also be
#' called on a `shrthnd_list()` object.
#'
#' @param x A `shrthnd_num()` vector
#'
#' @return A character vector
#'
#' @seealso [shrthnd_num()]
#' @family tag
#'
#' @export
#' @rdname shrthnd_tags
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' shrthnd_tags(sh_x)
#' shrthnd_unique_tags(sh_x)
shrthnd_tags <- function(x) {
  UseMethod("shrthnd_tags")
}

#' @export
shrthnd_tags.shrthnd_num <- function (x, ...) {
  vctrs::field(x, "tag")
}

#' @export
shrthnd_tags.shrthnd_list <- function (x, ...) {
  names(x)
}

#' @export
#' @rdname shrthnd_tags
shrthnd_unique_tags <- function(x) {
  UseMethod("shrthnd_unique_tags")
}

#' @export
shrthnd_unique_tags.shrthnd_num <- function(x, ...) {
  tags <- unique(vctrs::field(x, "tag"))
  tags[!is.na(tags)]
}

#' @export
shrthnd_unique_tags.shrthnd_list <- function(x, ...) {
  names(x)
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
