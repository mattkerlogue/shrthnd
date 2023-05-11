#' Collect the shorthand in a vector
#'
#' `shrthnd_list()` generates a lookup table of shorthand markers in a vector,
#' either a character vector containing shorthand or a `shrthnd_num()` vector.
#'
#' @param x A character vector containing shorthand
#' @param shorthand A character vector of shorthand values
#' @param na_values A character value of NA values
#'
#' @return A list of shorthand positions in a vector
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' shrthnd_list(x, c("[c]", "[e]"))
shrthnd_list <- function(x, shorthand = NULL, na_values = "NA") {

  if (is_shrthnd_num(x)) {

    shrt_values <- shrthnd_tags(x)
    what_shrthnd <- shrthnd_tags_unique(x)

  } else if (!rlang::is_bare_character(x)) {

    cli::cli_abort("{.arg x} must be a character vector")

  } else {

    shrt_values <- gsub("(^[-\\(]?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?\\)?)(.*$)", "\\5", x)
    shrt_values <- gsub(shrthnd_regex(na_values), "", shrt_values)
    what_shrthnd <- unique(shrt_values[(shrt_values != "") & !is.na(shrt_values)])

    if (!is.null(shorthand)) {
      if (sum(!(what_shrthnd %in% shorthand)) > 0) {
        cli::cli_alert_warning(
          "{.arg x} contains non-numeric values not in {.arg shorthand}"
        )
        return(NULL)
      }
    }

  }

  where_shrthnd <- purrr::map(
    what_shrthnd, ~which(shrt_values == .x, useNames = FALSE)
  )

  new_shrthnd_list(what_shrthnd, where_shrthnd)

}

new_shrthnd_list <- function(s = character(), w = list()) {

  if (!rlang::is_bare_character(s)) {
    cli::cli_abort("{.arg s} must be a character vector")
  }

  vctrs::list_check_all_vectors(w)

  sl <- vctrs::as_list_of(w)
  sl <- vctrs::vec_set_names(sl, s)

  vctrs::new_vctr(sl, class = "shrthnd_list")

}

#' @export
vec_ptype_abbr.shrthnd_list <- function (x, ...) {
  "sh_lst"
}

#' @export
print.shrthnd_list <- function (x, ...) {
  sl_s <- names(x)
  sl_c <- unname(lengths(x))

  cat(paste0("<shrthnd_list[", length(x), "]>" ), "\n")
  for (i in seq_along(x)) {
    sl_w <- x[[i]]
    loc <- ifelse(length(sl_w) > 1, " locations)", " location)")
    if (length(sl_w) > 5) {
      sl_w <- paste0(glue::glue_collapse(sl_w[1:5], sep = ","), "...")
    } else {
      sl_w <- glue::glue_collapse(sl_w, sep = ", ")
    }


    cat(
      paste0(sl_s[i], " ",
             pillar::style_subtle(paste0("(", sl_c[i], loc)), ":"),
      sl_w, "\n"
    )

  }

}
