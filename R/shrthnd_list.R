#' List the shorthand in a vector
#'
#' `shrthnd_list()` generates a lookup table of shorthand markers in a vector,
#' either a character vector containing shorthand or a `shrthnd_num()` vector.
#'
#' @param x A character vector containing shorthand, or a `shrthnd_num()` vector
#' @param shorthand A character vector of shorthand values to validate tags against
#' @param na_values A character value of NA values to ignore
#' @param dec The decimal separator for numbers
#' @param bigmark The separator to the left of the decimal separator
#'
#' @return A list of shorthand positions in a vector
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' shrthnd_list(x)
#'
#' sh_x <- shrthnd_num(x)
#' sh_x
#' shrthnd_list(sh_x)
shrthnd_list <- function(x, shorthand = NULL, na_values = c("", "NA"),
                         dec = ".", bigmark = ",") {
  UseMethod("shrthnd_list")
}

#' @export
shrthnd_list.shrthnd_num <- function(x, ...) {

  tags <- shrthnd_tags(x)
  unq_tags <- shrthnd_unique_tags(x)
  where_shrthnd <- find_tag_locations(tags, unq_tags)

  new_shrthnd_list(unq_tags, where_shrthnd)

}

#' @export
shrthnd_list.character <- function(x, shorthand = NULL, na_values = c("", "NA"),
                                   dec = ".", bigmark = ",", ...) {

  if (!rlang::is_bare_character(x)) {
    cli::cli_abort("{.arg x} must be a character vector")
  }

  if (sum(grepl("\\d+", x)) == 0) {
    cli::cli_alert_warning(
      "{.arg x} does not contain numeric content"
    )
    return(NULL)
  }

  if (!rlang::is_bare_character(na_values)) {
    cli::cli_abort("{.arg na_values} must be a character vector")
  }

  all_tags <- extract_text(x, na_values, dec, bigmark)
  unq_tags <- unique(all_tags)
  unq_tags <- unq_tags[(unq_tags!= "") & !is.na(unq_tags)]

  if (!is.null(shorthand)) {

    if (!rlang::is_bare_character(shorthand)) {
      cli::cli_abort("{.arg shorthand} must be a character vector")
    }

    if (!validate_tags(unq_tags, shorthand)) {
      cli::cli_alert_warning(
        "{.arg x} contains non-numeric values not in {.arg shorthand}"
      )
      return(NULL)
    }

  }

  where_shrthnd <- find_tag_locations(all_tags, unq_tags)

  if (length(where_shrthnd) == 0) {
    cli::cli_alert_warning(
      "No shorthand detected in {.arg x}"
    )
    return(NULL)
  }

  new_shrthnd_list(unq_tags, where_shrthnd)

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
