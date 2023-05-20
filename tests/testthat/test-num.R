x <- c("12", "34.567", "[c]", "NA", "56.78 [e]", "78.9", "90.123[e]")
y <- c("12", "34", "[c]", "NA", "56 [e]", "78", "90[e]")
sh_x <- shrthnd_num(x)
sh_x3 <- shrthnd_num(x, digits = 3L)
sh_y <- shrthnd_num(y)

test_that("sh_dbl generation", {
  expect_type(shrthnd_num(x), "list")
  expect_s3_class(shrthnd_num(x), "shrthnd_num")
  expect_length(shrthnd_num(x), 7)
  expect_equal(attr(sh_x, "digits"), 2L)
  expect_equal(attr(sh_x3, "digits"), 3L)
  expect_snapshot(shrthnd_num(x))
  expect_snapshot(tibble::tibble(x = x, sh_x = sh_x))
})

test_that("sh_int generation", {
  expect_type(shrthnd_num(y), "list")
  expect_s3_class(shrthnd_num(y), "shrthnd_num")
  expect_length(shrthnd_num(y), 7)
  expect_equal(attr(sh_y, "digits"), NULL)
  expect_snapshot(shrthnd_num(y))
  expect_snapshot(tibble::tibble(y = y, sh_y = sh_y))
})

test_that("shrthnd_num errors", {
  expect_error(shrthnd_num(1), regexp = "`x` must be a character vector")
  expect_error(shrthnd_num(list()), regexp = "`x` must be a character vector")
  expect_error(shrthnd_num(c("sd123")),
               regexp = "unable to convert `x` to a numeric vector")
  expect_snapshot(shrthnd_num("a"), error = TRUE)
  expect_snapshot(shrthnd_num(c("123", "567")), error = TRUE)
})

test_that("make_shrthnd_num", {
  num_int <- c(12, 34, 56, 78, 90)
  num_dbl <- c(12.34, 34.567, 56, 78, 90)
  tags <- c(NA, NA, "[c]", "[e]", "")
  sh_dbl3 <- make_shrthnd_num(num_dbl, tags, digits = 3L)
  expect_type(make_shrthnd_num(num_int, tags), "list")
  expect_s3_class(make_shrthnd_num(num_dbl, tags), "shrthnd_num")
  expect_s3_class(make_shrthnd_num(num_int, tags), "shrthnd_num")
  expect_equal(attr(sh_dbl3, "digits"), 3L)
})

test_that("make_shrthnd_num errors", {
  expect_error(make_shrthnd_num("a"))
  expect_error(make_shrthnd_num(1, 1))
  expect_error(make_shrthnd_num(1))
  expect_error(make_shrthnd_num(1, "a", "a"))
})
