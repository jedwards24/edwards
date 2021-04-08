test_that("mode_stat() works", {
  x <- c(1:5, 2L, 3L)
  expect_equal(mode_stat(x, F), 2L)
  expect_equal(mode_stat(x, T), c(2L, 3L))
})

test_that("factor_to_numeric() works", {
  x1 <- c(TRUE, FALSE, TRUE)
  x2 <- c(0.4, 1.5, -1)
  x3 <- 9:3
  y <- c(1L, 0L, 1L)
  expect_identical(factor_to_numeric(factor(x1)), y)
  expect_identical(factor_to_numeric(factor(x1, levels = c(TRUE, FALSE))), y)
  expect_identical(factor_to_numeric(factor(x2)), x2)
  expect_equal(factor_to_numeric(factor(x3)), x3)
})

test_that("need() works", {
  expect_error(need("fakePackage"), "Package \"fakePackage\" needed")
  expect_silent(need("stats")) # Assumes stats package is always installed
})
