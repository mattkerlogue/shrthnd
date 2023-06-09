% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_annotations.R
\name{find_annotations}
\alias{find_annotations}
\alias{guess_annotations}
\title{Find annotations in a data frame}
\usage{
find_annotations(
  df,
  type = c("sheet", "cells"),
  title_first = TRUE,
  guess_source = TRUE,
  .row_var = row,
  .col_var = col,
  .value_var = value
)

guess_annotations(
  df,
  type = c("sheet", "cells"),
  .row_var = row,
  .col_var = col,
  .value_var = value
)
}
\arguments{
\item{df}{A data frame object}

\item{type}{Whether the data frame is in "sheet" format or "cells" format}

\item{title_first}{Whether the first annotation should be treated as the
table title}

\item{guess_source}{Whether to guess a source note from the annoations}

\item{.row_var}{When using \code{type = "cells"} the name of the variable with
row positions}

\item{.col_var}{When using \code{type = "cells"} the name of the variable with
column positions}

\item{.value_var}{When using \code{type = "cells"} the name of the variable with
row positions}
}
\description{
\code{find_annotations()} takes a data frame and identifies possible annotations
contained within it and returns them as a named list. \code{guess_annotations()}
is a low-level helper that extracts annotations and returns them as a tibble
of cell values, row and column positions.
}
\details{
Data frames have a declared \code{type}, which must be either \code{"sheet"} format
(the default) or \code{"cells"} format. \code{"sheet"} format is a standard
two-dimensional data frame format, such as those read in by
\code{base::read.csv()} or \code{readxl::read_excel()}. \code{"cells"} format is for
data frames where each row represents a cell from a spreadsheet and contains
a variable for the cell's value, and separate variables providing the row
and column variable.

By default \code{find_annotations()} will try to help parse the annotations found
by \code{guess_annotations()}. With \code{title_first = TRUE}, the first annotation
found in a data frame is assumed to provide a title or label for the table
contained in the data frame. With \code{guess_source = TRUE}, the annotations
will be searched for one starting with either \code{"Source:"}, \code{"Data source:"}
or \code{"Source data:"}.

When using \code{type = "cells"} the variables identifying the row, column and
cell values are specified by \code{.row_var}, \code{.col_var} and \code{.value_var}
respectively.
}
\examples{
example_df <- tibble::tibble(
  col1 = c(
    "Table 1", "An example sheet", "species", "Adelie", "Gentoo", "Chinstrap",
    "This table is based on data in the palmerpenguins R package",
    "Source: {palmerpenguins} R package"
  ),
  col2 = c(NA_character_, NA_character_, "bill_length_mm", "38.791",
           "47.505", "48.834", NA_character_, NA_character_),
  col3 = c(NA_character_, NA_character_, "bill_depth_mm", "18.346",
           "14.982", "18.421", NA_character_, NA_character_)
)

example_df

find_annotations(example_df)

guess_annotations(example_df)

}
