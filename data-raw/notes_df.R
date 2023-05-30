# data frames for testing find_notes / guess_notes

df_cells <- palmerpenguins::penguins |>
  dplyr::summarise(
    across(c(bill_length_mm, bill_depth_mm, flipper_length_mm),
           ~round(mean(.x, na.rm = TRUE), digits = 3)),
    .by = c(species)
  ) |>
  tidyr::pivot_longer(cols = -species) |>
  dplyr::mutate(
    row = dplyr::case_match(species,
                            "Adelie" ~ 4, "Gentoo" ~5, "Chinstrap" ~ 6),
    col = dplyr::case_match(name,
                            "bill_length_mm" ~ 2, "bill_depth_mm" ~3,
                            "flipper_length_mm" ~ 4),
    value = as.character(value)
  ) |>
  dplyr::select(-species, -name) |>
  tibble::add_row(
    value = c("species", "bill_length_mm", "bill_depth_mm", "flipper_length_mm"),
    row = 3, col = 1:4
  ) |>
  tibble::add_row(
    value = c("Adelie", "Gentoo", "Chinstrap"),
    row = 4:6, col = 1
  ) |>
  dplyr::add_row(
    value = c(
      "Table 1", "This is an example table",
      "The data in this table is generated from the palmerpenguins R package.",
      "Run citation('palmerpenguins') to see full citation details.",
      paste("Source: Horst, Hill & Gorman (2020), palmerpenguins:",
            "Palmer Archipelago (Antarctica) penguin data. R package",
            "https://doi.org/10.5281/zenodo.3960218")),
    row = c(1:2, 8:10), col = 1
  ) |>
  dplyr::add_row(
    value = NA_character_,
    row = 7, col = 1:4
  ) |>
  dplyr::arrange(row, col)

df_sheet <- df_cells |>
  dplyr::mutate(col = paste0("col", col)) |>
  tidyr::pivot_wider(names_from = col, values_from = value) |>
  dplyr::select(-row)
