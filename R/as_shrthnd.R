#' Coerce a shrthnd_num to a character vector with shorthand
#'
#' `as_shrthnd()` coerces a `shrthnd_num()` vector back to a character vector
#' re-inserting the shorthand tags.
#'
#' When calling `as.character()` on a `shrthnd_num()` the output is as you
#' would expect when calling it on a traditional numeric vector, `as_shrthnd()`
#' returns a character vector combining the numeric vector and the shorthand
#' tags.
#'
#' @param x A [shrthnd_num()] vector
#' @param digits Whether to apply digit formatting
#' @param .pillar A flag for formatting within the `{pillar}` package
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"), digits = 1)
#' as_shrthnd(sh_x)
#' as_shrthnd(sh_x, digits = FALSE)
as_shrthnd <- function(x, digits = TRUE, .pillar = FALSE) {

  num <- field(x, "num")
  tag <- field(x, "tag")

  if (digits) {
    d <- attr(x, "digits")
    if (!is.null(d)) {
      out <- formatC(num, digits = d, format = "f", big.mark = ",")
    } else {
      out <- formatC(num, format = "d", big.mark = ",")
    }
  } else {
    out <- formatC(num, format = "d", big.mark = ",")
  }

  is_na <- is.na(num)
  has_sh <- !is.na(tag)

  out[is_na & !has_sh] <- NA_character_

  if (.pillar) {
    out[is_na & has_sh] <- pillar::style_subtle(tag[is_na & has_sh])
    out[!is_na & has_sh] <- paste0(
      out[!is_na & has_sh],
      pillar::style_subtle(tag[!is_na & has_sh])
    )
  } else {
    out[is_na & has_sh] <- tag[is_na & has_sh]
    out[!is_na & has_sh] <- paste0(out[!is_na & has_sh], tag[!is_na & has_sh])
  }

  return(out)

}

