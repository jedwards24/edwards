context("Counting functions")

x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), c = 1:3)

test_that("count_string is correct", {
  expect_identical(count_string(x, "an"), c(a = 3L))
  expect_identical(count_string(x, "an", all = T), c(a = 3L, b = 0L, c = 0L))
  expect_identical(count_string(x, "nun", all = T), c(a = 0L, b = 0L, c = 0L))
  expect_identical(length(count_string(x, "un")), 0L)
  expect_message(count_string(x, "un"), "not found in data")
  expect_identical(length(count_string(x, "1")), 0L)
})

test_that("count_matches is correct", {
  expect_identical(count_matches(x, "an"), c(a = 1L))
  expect_message(count_matches(x, "bo"), "No matches in data")
  expect_identical(count_matches(x, 1L), c(c = 1L))
  expect_identical(length(count_matches(x, 1)), 0L)
  expect_identical(length(count_matches(x, "1")), 0L)
})

test_that("count_nas is correct", {
  mt <- count_nas(mtcars)
  expect_message(count_nas(mtcars), "no NAs in the data")
  expect_identical(length(mt), 0L)
  expect_true(is.integer(mt))
  expect_gt(sum(count_nas(airquality)), 0)
})
