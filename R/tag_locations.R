#' Get tag locations
#'
#' Base R's matching and location functions will work directly with the
#' numeric component of a `shrthnd_num()` vector, these functions provide
#' the same functionality but applied to the tag component.
#'
#' `tag_match()` and `tag_in()` are wrappers around `vctrs::vec_match()` and
#' `vctrs::vec_in()` and thus equivalent to `match()` and `%in%` as applied
#' to the tag components of a `shrthnd_num()`. `tag_match()` will return an
#' integer vector showing the first location of the tag provided, `tag_in()`
#' will return `TRUE` or `FALSE` depending on whether the tag is in the
#' vector's shorthand.
#'
#' `where_tag()` is equivalent to computing `tags == tag`, `any_tag()` is
#' equivalent to `is.na(tags)`, while `no_tag()` is equivalent to `is.na(tags)`.
#' They return a logical vector the same length as `x`.
#'
#' `locate_tag()`, `locate_any_tag()`, `located_no_tag()` are equivalent to
#' passing the return values of `where_tag()`, `any_tag()` and `no_tag()` to
#' `which()`. They return an integer vector the same length as `x`.
#'
#' @param x A `shrthnd_num()` vector
#' @param tag A single tag to locate
#'
#' @return For `tag_match()`, `locate_tag()`, `locate_any_tag()` and
#' `locate_no_tag()` an integer vector. For `tag_in()`, `where_tag()`,
#' `any_tag()` and `no_tag()` a logical vector.
#'
#' @family tag
#'
#' @rdname tag_locations
#' @export
#'
#' @examples
#' x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
#' sh_x <- shrthnd_num(x, c("[c]", "[e]"))
#' shrthnd_tags(sh_x)
#'
#' tag_match(sh_x, "[e]")
#'
#' tag_in(sh_x, "[e]")
#'
#' where_tag(sh_x, "[e]")
#'
#' any_tag(sh_x)
#'
#' no_tag(sh_x)
#'
#' locate_tag(sh_x, "[e]")
#'
#' locate_any_tag(sh_x)
#'
#' locate_no_tag(sh_x)
tag_match <- function(x, tag) {

  if (!rlang::is_scalar_character(tag)) {
    cli::cli_abort("{.arg tag} must be a character vector of length 1")
  }

  tags <- shrthnd_tags(x)

  vctrs::vec_match(tag, tags, needles_arg = "tag", haystack_arg = "x")

}

#' @rdname tag_locations
#' @export
tag_in <- function(x, tag) {

  if (!rlang::is_scalar_character(tag)) {
    cli::cli_abort("{.arg tag} must be a character vector of length 1")
  }

  tags <- shrthnd_tags(x)

  vctrs::vec_in(tag, tags, needles_arg = "tag", haystack_arg = "x")

}

#' @rdname tag_locations
#' @export
where_tag <- function(x, tag) {

  tag_locator(x, tag, "where_tag")

}

#' @rdname tag_locations
#' @export
any_tag <- function(x) {

  tag_locator(x, NULL, "any_tag")

}

#' @rdname tag_locations
#' @export
no_tag <- function(x) {

  tag_locator(x, NULL, "no_tag")

}

#' @rdname tag_locations
#' @export
locate_tag <- function(x, tag) {

  tag_markers <- tag_locator(x, tag, "where_tag")

  which(tag_markers)

}

#' @rdname tag_locations
#' @export
locate_any_tag <- function(x) {

  tag_markers <- tag_locator(x, NULL, "any_tag")

  which(tag_markers)

}

#' @rdname tag_locations
#' @export
locate_no_tag <- function(x) {

  tag_markers <- tag_locator(x, NULL, "no_tag")

  which(tag_markers)

}

tag_locator <- function(x, tag, type) {

  if (!is.null(tag) & !rlang::is_scalar_character(tag)) {
    cli::cli_abort("{.arg tag} must be a character vector of length 1")
  }

  tags <- shrthnd_tags(x)

  switch(
    type,
    where_tag = tags == tag,
    any_tag = !is.na(tags),
    no_tag = is.na(tags)
  )

}
