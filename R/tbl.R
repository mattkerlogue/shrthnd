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

  if (is.null(title) & is.null(source_note) & is.null(notes)) {
    cli::cli_warn(
      "{.arg {c(\"title\", \"source_note\", \"notes\")}} are all NULL, returning
      as a bare {.fun tibble::tibble}"
    )
    tibble::new_tibble(x)
  } else {
    tibble::new_tibble(x, shrthnd_title = title, shrthnd_source = source_note,
                       shrthnd_notes = notes, class = "shrthnd_tbl")
  }

}

#' @export
tbl_sum.shrthnd_tbl <- function(x) {
  c("Title" = attr(x, "shrthnd_title"),
    "A tibble" = pillar::dim_desc(x))
}

#' @export
tbl_format_footer.shrthnd_tbl <- function(x, setup, ...) {

  notes <- attr(x, "shrthnd_notes")
  source_note <- attr(x, "shrthnd_source")

  nnotes <- length(notes)

  base_footer <- NextMethod()

  if (!is.null(notes)) {
    shrthnd_notes <- paste(
      "#",
      cli::symbol$star,
      cli::pluralize(
        "There {cli::qty(nnotes)}{?is/are} {nnotes} note{?s}, use `shrthnd_tbl_notes(x)` to view"
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

  c(base_footer, pillar::style_subtle(c(shrthnd_notes, shrthnd_source)))

}
