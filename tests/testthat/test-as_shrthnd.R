x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
sh_x <- shrthnd_num(x)
sh_x3 <- shrthnd_num(x, digits = 3)
y <- c("12", "34", "[c]", "NA", "56[e]", "78", "90[e]")
sh_y <- shrthnd_num(y)

test_that("as_shrthnd generation", {
  expect_type(as_shrthnd(sh_x), "character")
  expect_type(as_shrthnd(sh_y), "character")
  expect_length(as_shrthnd(sh_x), 7)
  expect_length(as_shrthnd(sh_y), 7)
  expect_equal(
    as_shrthnd(sh_x),
    c("12.00", "34.57", "[c]", NA, "56.78[e]", "78.90", "90.12[e]")
  )
  expect_equal(
    as_shrthnd(sh_x, 3),
    c("12.000", "34.567", "[c]", NA, "56.780[e]", "78.900", "90.123[e]")
  )
  expect_equal(
    as_shrthnd(sh_x, 0),
    c("12", "35", "[c]", NA, "57[e]", "79", "90[e]")
  )
  expect_equal(
    as_shrthnd(sh_x3),
    c("12.000", "34.567", "[c]", NA, "56.780[e]", "78.900", "90.123[e]")
  )
  expect_equal(
    as_shrthnd(sh_y),
    c("12", "34", "[c]", NA, "56[e]", "78", "90[e]")
  )
  expect_equal(
    as_shrthnd(sh_y, 2),
    c("12", "34", "[c]", NA, "56[e]", "78", "90[e]")
  )
})

test_that("as_shrthnd for pillar", {
  expect_type(as_shrthnd(sh_x, .pillar = TRUE), "character")
  expect_length(as_shrthnd(sh_x, .pillar = TRUE), 7)
})

test_that("as_shrthnd errors", {
  expect_error(as_shrthnd(1), regexp = "`x` must be a <shrthnd_num>")
  expect_error(as_shrthnd("a"), regexp = "`x` must be a <shrthnd_num>")
  expect_error(as_shrthnd(list()), regexp = "`x` must be a <shrthnd_num>")
  expect_error(as_shrthnd(sh_x, digits = "a"),
               regexp = "`digits` must be a single integer")
  expect_error(as_shrthnd(sh_x, digits = 1:2),
               regexp = "`digits` must be a single integer")
})
