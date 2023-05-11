#' Get the shorthand values attached to a vector
#'
#' @param x A [shrthnd_num()] vector
#'
#' @return The shrthnd_list associated with x or a character vector of
#'  shorthand tags the same length as the input
#' @export
#' @rdname shrthnd_values
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' get_shrthnd_list(sh_x)
#' shrthnd_tags(sh_x)
get_shrthnd_list <- function(x) {
  if (!is_shrthnd_num(x)) {
    cli::cli_abort("{.arg x} is not a shrthnd_num")
  }

  attr(x, "shrthnd")

}

#' @export
#' @rdname shrthnd_values
shrthnd_tags <- function(x) {
  sl <- get_shrthnd_list(x)
  l <- length(x)
  gen_shrthnd(sl, l)
}

gen_shrthnd <- function(sl, l) {
  out <- character(l)
  sl_s <- names(sl)
  for (i in seq_along(sl)) {
    out[sl[[i]]] <- sl_s[i]
  }
  out[out == ""] <- NA_character_
  return(out)
}
