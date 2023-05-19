#' Convert general notes to tibble title or source note
#'
#' The `note_to_title()` and `note_to_source_note()` allow you to move a note
#' from inside the general `shrthnd_notes()` of a `shrthnd_tbl()` and move
#' them to the `shrthnd_title()` or `shrthnd_source_note()` of the tibble.
#'
#' @param x A `shrthnd_tbl()` object
#' @param note The number of the note to move
#' @param .overwrite Whether to overwrite existing
#'
#' @return A `shrthnd_tbl()`
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' tbl <- tibble::tibble(x = x, sh_x = sh_x)
#'
#' sh_tbl <- shrthnd_tbl(tbl) |>
#'   set_notes(c("Note 1", "Note 2", "Note 3")) |>
#'   note_to_title(1) |>
#'   note_to_source_note(2)
#'
#' sh_tbl
#'
#' shrthnd_notes(sh_tbl)
note_to_title <- function(x, note, .overwrite = FALSE) {

  chk_shrthnd_tbl(x)

  if (!rlang::is_scalar_integerish(note)) {
    cli::cli_abort("{.arg note} must be an integer of length 1")
  }

  notes <- shrthnd_notes(x)

  if (is.null(notes)) {
    cli::cli_abort("{.arg x} does not have any notes")
  }

  if (note < 1 || note > length(notes)) {
    cli::cli_abort("{.arg note} must be between 1 and {length(notes)}")
  }

  y <- set_tbl_attr(x, "title", value = notes[note], .overwrite)
  y <- set_tbl_attr(y, "notes", value = notes[-note], .overwrite = TRUE, .append = FALSE)

  return(y)

}

#' @rdname note_to_title
#' @export
note_to_source_note <- function(x, note, .overwrite = FALSE) {

  chk_shrthnd_tbl(x)

  if (!rlang::is_scalar_integerish(note)) {
    cli::cli_abort("{.arg note} must be an integer of length 1")
  }

  notes <- shrthnd_notes(x)

  if (is.null(notes)) {
    cli::cli_abort("{.arg x} does not have any notes")
  }

  if (note < 1 || note > length(notes)) {
    cli::cli_abort("{.arg note} must be between 1 and {length(notes)}")
  }

  y <- set_tbl_attr(x, "source_note", value = notes[note], .overwrite)
  y <- set_tbl_attr(y, "notes", value = notes[-note], .overwrite = TRUE, .append = FALSE)

  return(y)

}
