x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
sh_x <- shrthnd_num(x)

test_that("tags from shrthnd_num", {
  expect_type(shrthnd_tags(sh_x), "character")
  expect_length(shrthnd_tags(sh_x), 7)
  expect_equal(shrthnd_tags(sh_x), c(NA, NA, "[c]", NA, "[e]", NA, "[e]"))
})

test_that("unique tags from shrthnd_num", {
  expect_type(shrthnd_unique_tags(sh_x), "character")
  expect_length(shrthnd_unique_tags(sh_x), 2)
  expect_equal(shrthnd_unique_tags(sh_x), c("[c]", "[e]"))
})

test_that("tags from shrthnd_list", {
  expect_type(shrthnd_tags(shrthnd_list(x)), "character")
  expect_length(shrthnd_tags(shrthnd_list(x)), 2)
  expect_equal(shrthnd_tags(shrthnd_list(x)), c("[c]", "[e]"))
})

test_that("unique tags from shrthnd_list", {
  expect_type(shrthnd_unique_tags(shrthnd_list(x)), "character")
  expect_length(shrthnd_unique_tags(shrthnd_list(x)), 2)
  expect_equal(shrthnd_unique_tags(shrthnd_list(x)), c("[c]", "[e]"))
})

test_that("tag errors", {
  expect_error(shrthnd_tags("a"))
  expect_error(shrthnd_tags(1:10))
  expect_error(shrthnd_tags(list()))
  expect_error(shrthnd_unique_tags("a"))
  expect_error(shrthnd_unique_tags(1:10))
  expect_error(shrthnd_unique_tags(list()))
})
