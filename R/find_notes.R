#' Find notes in a data frame
#'
#' `find_notes()` takes a data frame and identifies possible notes contained
#' within it and returns them as a named list. `guess_notes()` is a low-level
#' helper that extracts notes and returns them as a tibble of cell values,
#' row and column positions.
#'
#' Data frames have a declared `type`, which must be either `"sheet"` format
#' (the default) or `"cells"` format. `"sheet"` format is a standard
#' two-dimensional data frame format, such as those read in by
#' `base::read.csv()` or `readxl::read_excel()`. `"cells"` format is for
#' data frames where each row represents a cell from a spreadsheet and contains
#' a variable for the cell's value, and separate variables providing the row
#' and column variable.
#'
#' By default `find_notes()` will try to help parse the notes found by
#' `guess_notes()`. With `title_first = TRUE`, the first note found in a data
#' frame is assumed to provide a title or label for the table contained in
#' the data frame. With `guess_source = TRUE`, the notes will be searched for
#' one starting with either `"Source:"`, `"Data source:"` or `"Source data:"`.
#'
#' When using `type = "cells"` the variables identifying the row, column and
#' cell values are specified by `.row_var`, `.col_var` and `.value_var`
#' respectively.
#'
#'
#' @param df A data frame object
#' @param type Whether the data frame is in "sheet" format or "cells" format
#' @param title_first Whether the first note should be treated as the table
#'  title
#' @param guess_source Whether to guess a source note
#' @param .row_var When using `type = "cells"` the name of the variable with
#'  row positions
#' @param .col_var When using `type = "cells"` the name of the variable with
#'  column positions
#' @param .value_var When using `type = "cells"` the name of the variable with
#'  row positions
#'
#' @export
#'
#' @examples
#' example_df <- tibble::tibble(
#'   col1 = c(
#'     "Table 1", "An example sheet", "species", "Adelie", "Gentoo", "Chinstrap",
#'     "This table is based on data in the palmerpenguins R package",
#'     "Source: {palmerpenguins} R package"
#'   ),
#'   col2 = c(NA_character_, NA_character_, "bill_length_mm", "38.791",
#'            "47.505", "48.834", NA_character_, NA_character_),
#'   col3 = c(NA_character_, NA_character_, "bill_depth_mm", "18.346",
#'            "14.982", "18.421", NA_character_, NA_character_)
#' )
#'
#' example_df
#'
#' find_notes(example_df)
#'
#' guess_notes(example_df)
#'
find_notes <- function(df, type = c("sheet", "cells"), title_first = TRUE,
                       guess_source = TRUE, .row_var = row, .col_var = col,
                       .value_var = value) {

  df_notes <- guess_notes(
    df, type = type, .row_var = {{ .row_var }}, .col_var = {{ .col_var }},
    .value_var = {{ .value_var}}
  )

  out_notes <- df_notes$value

  tn <- NULL
  if (title_first) {
    tn <- out_notes[1]
    out_notes <- out_notes[-1]
  }

  source_guesses <- NULL
  if (guess_source) {
    source_guesses <- which(
      grepl("^([Dd]ata[-\\s]?)?[Ss]ource(\\s[Dd]ata[-\\s]?)?(\\s*?[:-]\\s*)(.*)", out_notes)
    )
  }

  sn <- NULL
  if (length(source_guesses) == 1) {
    sn <- out_notes[source_guesses]
    out_notes <- out_notes[-source_guesses]
  } else {
    cli::cli_warn(c("!" = "More than one source note found"))
  }

  notes_list <- new_shrthnd_notes_list(
    title = tn, source_note = sn, notes = out_notes,
    source_obj = rlang::as_string(rlang::call_args(rlang::current_call())[[1]]),
    .found = TRUE
  )

  return(notes_list)

}

#' @rdname find_notes
#' @export
guess_notes <- function(df, type = c("sheet", "cells"), .row_var = row,
                        .col_var = col, .value_var = value) {

  type <- rlang::arg_match(type)

  df <- tibble::as_tibble(df)

  if (type == "sheet") {

    if (dplyr::is_grouped_df(df)) {
      df <- dplyr::ungroup(df)
    }

    cell_df <- dplyr::mutate(df, {{ .row_var }} := dplyr::row_number())

    vv <- rlang::englue("{{ .value_var }}")

    cell_df <- tidyr::pivot_longer(cell_df, cols = -{{ .row_var }},
                                   values_to = vv,
                                   values_transform = as.character)

    cell_df <- dplyr::mutate(cell_df, {{ .col_var }} := dplyr::row_number(),
                             .by = {{ .row_var }})

    cell_df <- dplyr::select(cell_df, {{ .row_var }}, {{ .col_var }},
                             {{ .value_var }})

  } else if (type == "cells") {
    cell_df <- dplyr::transmute(
      df,
      row = {{ .row_var }}, col = {{ .col_var }}, value = {{ .value_var }}
    )
  }

  content_df <- dplyr::mutate(
    cell_df,
    has_content = !is.na(value)
  )

  content_rows <- dplyr::summarise(
    content_df,
    content_cells = sum(has_content),
    .by = row
  )

  note_rows <- content_rows$row[content_rows$content_cells == 1]

  note_guesses <- dplyr::filter(cell_df,
                                row %in% note_rows & !is.na(value))

  return(note_guesses)

}
