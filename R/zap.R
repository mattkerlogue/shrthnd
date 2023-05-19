#' Remove notes from tibbles
#'
#' The `zap_*()` functions remove notes from a `shrthnd_tbl()` object.
#' `zap_title()`, `zap_source_note()` and `zap_notes()` remove the title,
#' source note and general notes respectively. `zap_tbl()` removes all three
#' types of notes and also strips the `shrthnd_tbl` class from the object.
#' `zap_shrthnd()` is a low-level helper function that power the attribute
#' removal.
#'
#' To remove shrthnd from a vector use `as.numeric()`, `as.character()` or
#' `as_shrthnd()` to coerce the vector to another type.
#'
#' @param x A `shrthnd_tbl()`
#' @param what One or more of `title`, `source_note` or `notes` indicating
#'   which set of notes to remove
#' @param zap_class Whether to remove the "shrthnd_tbl" class
#'
#' @return Returns `x` with relevant attributes removed
#'
#' @family tbl
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' tbl <- tibble::tibble(x = x, sh_x = sh_x)
#' sh_tbl <- shrthnd_tbl(
#'   tbl,
#'   title = "Example table",
#'   notes = c("Note 1", "Note 2"),
#'   source_note = "Shrthnd documentation, 2023"
#' )
#'
#' sh_tbl
#'
#' zap_title(sh_tbl)
#'
#' zap_source_note(sh_tbl)
#'
#' zap_notes(sh_tbl)
#'
#' zap_tbl(sh_tbl)
#'
#' @name zap_shrthnd
NULL

#' @rdname zap_shrthnd
#' @export
zap_title <- function(x) {
  zap_shrthnd(x, "title", zap_class = FALSE)
}

#' @rdname zap_shrthnd
#' @export
zap_source_note <- function(x) {
  zap_shrthnd(x, "source_note", zap_class = FALSE)
}

#' @rdname zap_shrthnd
#' @export
zap_notes <- function(x) {
  zap_shrthnd(x, "notes", zap_class = FALSE)
}

#' @rdname zap_shrthnd
#' @export
zap_tbl <- function(x) {
  zap_shrthnd(x, c("title", "source_note", "notes"), zap_class = TRUE)
}

#' @rdname zap_shrthnd
#' @export
zap_shrthnd <- function(x, what = c("title", "source_note", "notes"),
                        zap_class = FALSE) {

  what <- rlang::arg_match(what, multiple = TRUE)

  attr_list <- list()

  if ("title" %in% what) {
    attr(x, "shrthnd_title") <- NULL
  }

  if ("source_note" %in% what) {
    attr(x, "shrthnd_source_note") <- NULL
  }

  if ("notes" %in% what) {
    attr(x, "shrthnd_notes") <- NULL
  }

  if (zap_class) {
    old_classes <- class(x)
    new_classes <- old_classes[old_classes != "shrthnd_tbl"]
    class(x) <- new_classes
  }

  return(x)

}
