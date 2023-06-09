#' Add annotations to tibbles
#'
#' `shrthnd_tbl()` provides a way to attach annotations to a table.
#' Specifically, it supports three types of annotation: a `title`, a
#' `source note` and general `notes`. The `title` and `source_note` are each
#' character vectors of length 1, while `notes` can be a character vector of
#' any length.
#'
#' @param tbl A `tibble::tibble()` or object that can be coerced to a tibble.
#' @param title A character vector for the title of `tbl`
#' @param source_note A character vector for a source note relating to `tbl`
#' @param notes A character vector of general notes relating to `tbl`
#'
#' @return A tibble with shrthnd annotations
#'
#' @seealso [is_shrthnd_tbl()]
#' @family tbl
#'
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
      cli::symbol$menu,
      cli::pluralize(
        paste0("There {cli::qty(nnotes)}{?is/are} {nnotes} note{?s}, ",
               "use `annotations(x)` to view")
      )
    )
  } else {
    shrthnd_notes <- NULL
  }

  if (!is.null(source_note)) {
    shrthnd_source <- paste(
      "#",
      cli::symbol$menu,
      "Source:",
      source_note
    )
  } else {
    shrthnd_source <- NULL
  }

  c(base_footer, pillar::style_subtle(c(shrthnd_source, shrthnd_notes)))

}
