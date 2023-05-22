x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
y <- c("12", "34", "[c]", "NA", "56[e]", "78", "90[e]")
sh_x <- shrthnd_num(x)
sh_y <- shrthnd_num(y)

test_that("addition", {
  expect_equal(sh_x + 2, c(14, 36.567, NA, NA, 58.78, 80.9, 92.123))
  expect_equal(sh_y + 2, c(14, 36, NA, NA, 58, 80, 92))
})

test_that("multiplication", {
  expect_equal(sh_x * 2, c(24, 69.134, NA, NA, 113.56, 157.8, 180.246))
  expect_equal(sh_y * 2, c(24, 68, NA, NA, 112, 156, 180))
})

test_that("divsion", {
  expect_equal(sh_x / 2, c(6, 17.2835, NA, NA, 28.39, 39.45, 45.0615))
  expect_equal(sh_y / 4, c(3, 8.5, NA, NA, 14, 19.5, 22.5))
})

test_that("summary generics", {
  expect_equal(prod(sh_x, na.rm = TRUE), 167475378)
  expect_equal(sum(sh_x, na.rm = TRUE), 272.37)
  expect_equal(any(sh_x == 12, na.rm = TRUE), TRUE)
  expect_equal(all(sh_x > 0, na.rm = TRUE), TRUE)
})

test_that("math generics", {
  expect_equal(ceiling(sh_x), c(12, 35, NA, NA, 57, 79, 91))
  expect_snapshot(sqrt(sh_x))
  expect_snapshot(sin(sh_x))
  expect_snapshot(log(sh_x))
})

test_that("stats", {
  expect_equal(mean(sh_x, na.rm = TRUE), 54.474)
  expect_equal(median(sh_x, na.rm = TRUE), 56.78)
  expect_equal(quantile(sh_x, na.rm = TRUE),
               c("0%" = 12, "25%" = 34.567, "50%" = 56.78, "75%" = 78.9,
                 "100%" = 90.123))
  expect_equal(sd(sh_x, na.rm = TRUE), 31.91053)
})

