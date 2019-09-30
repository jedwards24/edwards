context("Counting functions")

x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"))

test_that("count_string is correct", {
  expect_identical(count_string(x, "an"), c(a = 3L))
  expect_identical(count_string(x, "an", all = T), c(a = 3L, b = 0L))
  expect_identical(count_string(x, "nun", all = T), c(a = 0L, b = 0L))
  expect_identical(length(count_string(x, "un")), 0L)
})
