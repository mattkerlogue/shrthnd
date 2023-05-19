#' Add notes to tibbles
#'
#' @param tbl A `tibble::tibble()` or object that can be coerced to a tibble.
#' @param title A character vector representing the title of the tbl
#' @param notes A character vector of notes relating to the title
#' @param source_note A character vector for a source note
#'
#' @return A tibble with shrthnd annotations
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' tbl <- tibble::tibble(x = x, sh_x = sh_x)
#' shrthnd_tbl(
#'   tbl,
#'   title = "Example table",
#'   notes = c("Note 1", "Note 2"),
#'   source_note = "Shrthnd documentation, 2023"
#' )
shrthnd_tbl <- function(tbl, title = NULL, notes = NULL, source_note = NULL) {
  tbl <- tibble::as_tibble(tbl)
  new_shrthnd_tbl(tbl, title, notes, source_note)
}

new_shrthnd_tbl <- function(x, title = NULL, notes = NULL, source_note = NULL) {

  if (!tibble::is_tibble(x)) {
    cli::cli_abort("{.arg x} must be a {.fun tibble::tibble} object")
  }

  if (!rlang::is_scalar_character(title) && !is.null(title)) {
    cli::cli_abort("{.arg title} must be a character vector of length 1 or NULL")
  }

  if (!rlang::is_bare_character(notes) && !is.null(notes)) {
    cli::cli_abort("{.arg notes} must be a character vector or NULL")
  }

  if (!rlang::is_scalar_character(source_note) && !is.null(source_note)) {
    cli::cli_abort("{.arg source_note} must be a character vector of length 1 or NULL")
  }

  tibble::new_tibble(
    x, shrthnd_title = title, shrthnd_source_note = source_note,
    shrthnd_notes = notes, class = "shrthnd_tbl"
  )

}

#' @export
tbl_sum.shrthnd_tbl <- function(x) {
  c("Title" = attr(x, "shrthnd_title"),
    "A tibble" = pillar::dim_desc(x))
}

#' @export
tbl_format_footer.shrthnd_tbl <- function(x, setup, ...) {

  notes <- attr(x, "shrthnd_notes")
  source_note <- attr(x, "shrthnd_source_note")

  nnotes <- length(notes)

  base_footer <- NextMethod()

  if (!is.null(notes)) {
    shrthnd_notes <- paste(
      "#",
      cli::symbol$star,
      cli::pluralize(
        "There {cli::qty(nnotes)}{?is/are} {nnotes} note{?s}, use `shrthnd_notes(x)` to view"
      )
    )
  } else {
    shrthnd_notes <- NULL
  }

  if (!is.null(source_note)) {
    shrthnd_source <- paste(
      "#",
      cli::symbol$star,
      "Source:",
      source_note
    )
  } else {
    shrthnd_source <- NULL
  }

  c(base_footer, pillar::style_subtle(c(shrthnd_source, shrthnd_notes)))

}


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
