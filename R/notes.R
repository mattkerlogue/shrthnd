#' Get and set notes for a tibble
#'
#' A `shrthnd_tbl()` has three sets of notes that can be defined: a `title`,
#' a `source_note` and a set of general `notes`. This family of functions
#' allows you to set and modify these notes.
#'
#' `shrthnd_title()`, `shrthnd_source_note()` and `shrthnd_notes()` get the
#' relevant note(s) of a `shrthnd_tbl()` object. Passing a value to these
#' functions (`shrthnd_title(x) <- "My title"`) will set the value of these
#' notes, overwriting the existing value(s).
#'
#' `set_title()`, `set_source_note()`, and `set_notes()` also allow you to set
#' the value of these notes. By default they will not permit overwriting of
#' existing values, setting `.overwrite = TRUE` permits this.
#'
#' `add_notes()` allows you to append notes to the existing set of general
#' notes.
#'
#' `set_tbl_attr()` is a low level helper function that powers the assignment
#' operations.
#'
#' @param x A `shrthnd_tbl()` object
#' @param what Which note to set, one of `title`, `source_note` or `notes`
#' @param value The value to set
#' @param .overwrite Whether an existing value should be overwritten
#' @param .add When `what = "notes"`, whether to append to the existing
#'   set of notes
#' @param .add_before When adding notes, where to add the note (defaults to
#'   the end of the current set of notes)
#'
#' @return For `shrthnd_title()`, `shrthnd_source_note()` and `shrthnd_notes()`
#'   a character vector of the note(s). For the setting functions returns
#'   invisibly either `x` if the attribute was set or `NULL` if not.
#'
#' @family tbl
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' tbl <- tibble::tibble(x = x, sh_x = sh_x)
#'
#' sh_tbl <- shrthnd_tbl(tbl) |>
#'   set_title("My Example Table") |>
#'   set_source_note("Shrthnd Documentation, 2023") |>
#'   set_notes(c("Note 1", "Note 2"))
#'
#' sh_tbl
#'
#' shrthnd_title(sh_tbl)
#' shrthnd_source_note(sh_tbl)
#' shrthnd_notes(sh_tbl)
#'
#' add_notes(sh_tbl) <- "Note 3"
#' shrthnd_notes(sh_tbl)
#'
#' @name tbl_notes
NULL

#' @rdname tbl_notes
#' @export
shrthnd_title <- function(x) {
  chk_shrthnd_tbl(x)
  return(attr(x, "shrthnd_title"))
}

#' @rdname tbl_notes
#' @export
`shrthnd_title<-` <- function(x, value) {
  set_title(x, value, .overwrite = TRUE)
}

#' @rdname tbl_notes
#' @export
set_title <- function(x, value, .overwrite = FALSE) {
  set_tbl_attr(x, "title", value, .overwrite)
}

#' @rdname tbl_notes
#' @export
shrthnd_source_note <- function(x) {
  chk_shrthnd_tbl(x)
  return(attr(x, "shrthnd_source_note"))
}

#' @rdname tbl_notes
#' @export
`shrthnd_source_note<-` <- function(x, value) {
  set_source_note(x, value, .overwrite = TRUE)
}

#' @rdname tbl_notes
#' @export
set_source_note <- function(x, value, .overwrite = FALSE) {
  set_tbl_attr(x, "source_note", value, .overwrite)
}

#' @rdname tbl_notes
#' @export
shrthnd_notes <- function(x) {
  chk_shrthnd_tbl(x)
  return(attr(x, "shrthnd_notes"))
}

#' @rdname tbl_notes
#' @export
`shrthnd_notes<-` <- function(x, value) {
  set_notes(x, value, .overwrite = TRUE)
}

#' @rdname tbl_notes
#' @export
set_notes <- function(x, value, .overwrite = FALSE) {
  set_tbl_attr(x, "notes", value, .overwrite)
}

#' @rdname tbl_notes
#' @export
add_notes <- function(x, value, .add_before = Inf) {
  set_tbl_attr(x, "notes", value, .overwrite = TRUE, .add = TRUE,
               .add_before = .add_before)
}

#' @rdname tbl_notes
#' @export
`add_notes<-` <- function(x, value) {
  set_tbl_attr(x, "notes", value, .overwrite = TRUE, .add = TRUE,
               .add_before = Inf)
}

#' @rdname tbl_notes
#' @export
set_tbl_attr <- function(x, what = c("title", "source_note", "notes"),
                         value, .overwrite = FALSE, .add = FALSE,
                         .add_before = Inf) {

  chk_shrthnd_tbl(x)

  what <- rlang::arg_match(what)

  what_attr <- paste0("shrthnd_", what)

  curr_attr <- attr(x, what_attr)

  if (what_attr == "shrthnd_notes") {

    if(!rlang::is_character(value)) {
      cli::cli_abort("{.arg value} for {.arg {what_attr}} must be a character vector")
    }

    if(!rlang::is_scalar_logical(.add)) {
      cli::cli_abort("{.arg add} must be a logical vector of length 1")
    }

    if (.add) {

      if(!rlang::is_scalar_integerish(.add_before)) {
        cli::cli_abort("{.arg .add_before} must be an integer vector of length 1")
      }

      if (.add_before > length(curr_attr)) {
        .add_before <- Inf
      }

      if (.add_before == Inf) {
        attr(x, what_attr) <- c(curr_attr, value)
      } else if (.add_before == 0L | .add_before == 1L) {
        attr(x, what_attr) <- c(value, curr_attr)
      } else {
        attr(x, what_attr) <- c(
          curr_attr[1:(.add_before-1)],
          value,
          curr_attr[.add_before:length(curr_attr)]
        )
      }

      return(x)

    }

  } else {

    if (!rlang::is_scalar_vector(value)) {
      cli::cli_abort("{.arg value} for {.arg {what_attr}} must be a character vector of length 1")
    }

  }

  if(!rlang::is_scalar_logical(.overwrite)) {
    cli::cli_abort("{.arg overwrtie} must be a logical vector of length 1")
  }

  if (is.null(curr_attr)) {

    attr(x, what_attr) <- value
    return(x)

  } else if (.overwrite) {

    attr(x, what_attr) <- value
    return(x)

  } else {

    info_text <- paste0(
      "Use {.fun shrthnd::set_", what, "} or {.fun shrthnd::set_tbl_attr} ",
      "with {.code .overwrite = TRUE}"
    )

    cli::cli({
      cli::cli_alert_danger("{.arg {what_attr}} is already set and will not be overwritten", TRUE)
      cli::cli_alert_warning("current {.arg {what}}: {.val {curr_attr}}", wrap = TRUE)
      cli::cli_alert_info(info_text, wrap = TRUE)
    })

    return(invisible(NULL))

  }

}
