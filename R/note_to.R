#' Move notes to and from the title/source note of a tibble
#'
#' A `shrthnd_tbl()` has three sets of [notes][tbl_notes], the
#' `note_to_*()` functions allow you to move a general note to either the
#' title or source note of a tibble. the `*_to_notes()`functions do the
#' opposite and (re)insert either the title and/or source note back into the
#' general notes.
#'
#' For `title_to_notes()` and `title_source_to_notes()` the default is to
#' (re)insert the note at the start of the set of notes, for
#' `source_to_notes()` the default is to (re)insert the note at the end of
#' the set of notes.
#'
#' @param x A `shrthnd_tbl()` object
#' @param note The number of the note to move
#' @param .overwrite Whether to overwrite existing
#' @param .add_before Where to (re)insert the note
#'
#' @return A `shrthnd_tbl()`
#'
#' @family tbl
#'
#' @rdname note_to
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
#'
#' sh_tbl <- sh_tbl |>
#'   title_to_notes()
#'
#' shrthnd_notes(sh_tbl)
#'
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
  y <- set_tbl_attr(y, "notes", value = notes[-note], .overwrite = TRUE, .add = FALSE)

  return(y)

}

#' @rdname note_to
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

  sn <- notes[note]
  sn <- gsub("^([Dd]ata[-\\s]?)?[Ss]ource(\\s*?[:-]\\s*)(.*)", "\\3", sn, perl = TRUE)

  y <- set_tbl_attr(x, "source_note", value = sn, .overwrite)
  y <- set_tbl_attr(y, "notes", value = notes[-note], .overwrite = TRUE, .add = FALSE)

  return(y)

}

#' @rdname note_to
#' @export
title_to_notes <- function(x, .add_before = 0) {

  tn <- shrthnd_title(x)

  y <- set_tbl_attr(x, "notes", tn, .overwrite = TRUE, .add = TRUE,
                    .add_before = .add_before)

  y <- zap_title(y)

  return(y)

}

#' @rdname note_to
#' @export
source_to_notes <- function(x, .add_before = Inf) {

  sn <- shrthnd_source_note(x)

  if (!grepl("^([Dd]ata[-\\s]?)?[Ss]ource(\\s*?[:-]\\s*)", sn)) {
    sn <- paste("Source:", sn)
  }

  y <- set_tbl_attr(x, "notes", sn, .overwrite = TRUE, .add = TRUE,
                    .add_before = .add_before)

  y <- zap_source_note(y)

  return(y)

}

#' @rdname note_to
#' @export
title_source_to_notes <- function(x, .add_before = 0) {

  tn <- shrthnd_title(x)
  sn <- shrthnd_source_note(x)

  if (!grepl("^([Dd]ata[-\\s]?)?[Ss]ource(\\s*?[:-]\\s*)", sn)) {
    sn <- paste("Source:", sn)
  }

  y <- set_tbl_attr(x, "notes", c(tn, sn), .overwrite = TRUE, .add = TRUE,
                    .add_before = .add_before)

  y <- zap_title(y)
  y <- zap_source_note(y)

  return(y)

}

