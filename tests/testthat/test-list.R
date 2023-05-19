x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
sh_x <- shrthnd_num(x)

test_that("shrthnd_list generation", {
  expect_type(shrthnd_list(x), "list")
  expect_s3_class(shrthnd_list(x), "shrthnd_list")
  expect_length(shrthnd_list(x), 2)
  expect_named(shrthnd_list(x), c("[c]", "[e]"))
  expect_snapshot(shrthnd_list(x))
})

test_that("shrthnd_list from shrthnd_num", {
  expect_type(shrthnd_list(sh_x), "list")
  expect_s3_class(shrthnd_list(sh_x), "shrthnd_list")
  expect_length(shrthnd_list(sh_x), 2)
  expect_named(shrthnd_list(sh_x), c("[c]", "[e]"))
  expect_snapshot(shrthnd_list(sh_x))
})

test_that("shrthnd_list nulls", {
  expect_equal(suppressMessages(shrthnd_list("a")), NULL)
  expect_message(shrthnd_list("a"))
  expect_equal(
    suppressMessages(shrthnd_list(c("12", "34[a]"), shorthand = "[b]")),
    NULL
  )
  expect_message(
    shrthnd_list(c("12", "34[a]"), shorthand = "[b]"),
    regexp = "contains non-numeric values not in"
  )
  expect_equal(suppressMessages(shrthnd_list(c("12", "34"))), NULL)
  expect_message(shrthnd_list(c("12", "34")), regexp = "No shorthand detected")
})

test_that("shrthnd_list errors", {
  expect_error(shrthnd_list(1:10))
  expect_error(shrthnd_list(x, shorthand = 1))
  expect_error(shrthnd_list(x, na_values = 1))
})
