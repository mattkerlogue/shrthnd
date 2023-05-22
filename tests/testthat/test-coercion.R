x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
y <- c("12", "34", "[c]", "NA", "56[e]", "78", "90[e]")
sh_x <- shrthnd_num(x)
sh_y <- shrthnd_num(y)

test_that("sh_num to numeric", {
  expect_type(as.numeric(sh_x), "double")
  expect_length(as.numeric(sh_x), 7)
  expect_equal(as.numeric(sh_x), c(12, 34.567, NA, NA, 56.78, 78.9, 90.123))
  expect_type(as.numeric(sh_y), "double")
  expect_length(as.numeric(sh_y), 7)
  expect_equal(as.numeric(sh_y), c(12, 34, NA, NA, 56, 78, 90))
})

test_that("sh_num to character", {
  expect_type(as.character(sh_x), "character")
  expect_length(as.character(sh_x), 7)
  expect_equal(as.character(sh_x),
               c("12", "34.567", NA, NA, "56.78", "78.9", "90.123"))
  expect_type(as.character(sh_y), "character")
  expect_length(as.character(sh_y), 7)
  expect_equal(as.character(sh_y), c("12", "34", NA, NA, "56", "78", "90"))
})
