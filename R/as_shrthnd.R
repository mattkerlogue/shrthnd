#' Coerce a shrthnd_num to a character vector with shorthand
#'
#' @param x A [shrthnd_num()] vector
#' @param digits Whether to apply digit formatting
#' @param .subtle Used for formatting with the `{pillar}` package
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"), digits = 1)
#' as_shrthnd(sh_x)
#' as_shrthnd(sh_x, digits = FALSE)
as_shrthnd <- function(x, digits = TRUE, .subtle = FALSE) {

  x_sh <- shrthnd_tags(x)

  if (digits) {
    d <- attr(x, "digits")
    if (!is.null(d)) {
      out <- sprintf(paste0("%-0.", d, "f"), vctrs::vec_data(x))
    } else {
      out <- as.character(x)
    }
  } else {
    out <- as.character(x)
  }

  is_na <- is.na(x)
  has_sh <- !is.na(x_sh)

  out[is_na & !has_sh] <- NA_character_

  if (.subtle) {
    out[is_na & has_sh] <- pillar::style_subtle(x_sh[is_na & has_sh])
    out[!is_na & has_sh] <- paste0(
      out[!is_na & has_sh],
      pillar::style_subtle(x_sh[!is_na & has_sh])
    )
  } else {
    out[is_na & has_sh] <- x_sh[is_na & has_sh]
    out[!is_na & has_sh] <- paste0(out[!is_na & has_sh], x_sh[!is_na & has_sh])
  }

  out

}
