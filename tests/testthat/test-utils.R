test_that("mode_stat() works", {
  x <- c(1:5, 2L, 3L)
  expect_equal(mode_stat(x, F), 2L)
  expect_equal(mode_stat(x, T), c(2L, 3L))
})

test_that("factor_to_numeric() works", {
  x1 <- c(TRUE, FALSE, TRUE)
  x2 <- c("T", "false", "True")
  x3 <- c(0.4, 1.5, -1)
  x4 <- 9:3
  y <- c(1, 0, 1)
  expect_identical(factor_to_numeric(factor(x1)), y)
  expect_identical(factor_to_numeric(factor(x1, levels = c(TRUE, FALSE))), y)
  expect_identical(factor_to_numeric(factor(x2)), y)
  expect_identical(factor_to_numeric(factor(x3)), x3)
  out <- factor_to_numeric(factor(x4))
  expect_equal(out, x4)
  expect_type(out, "double")
})

test_that("extend_vector() works", {
  expect_identical(extend_vector(2:4, 5), c(2:4, NA, NA))
  expect_identical(extend_vector(2:4, 2), 2:4)
})

test_that("min_n() and max_n() work", {
  x <- c(4:6, NA)
  expect_error(min_n(x, 5), "must be between")
  expect_error(max_n(x, 5), "must be between")
  expect_equal(min_n(x, 1:4), x)
  expect_equal(max_n(x, 1:4), c(6:4, NA))
  expect_equal(min_n(x, 3:4), c(6L, NA))
  expect_equal(max_n(x, 3:4), c(4L, NA))
})

test_that("need() works", {
  expect_error(need("fakePackage"), "Package \"fakePackage\" needed")
  expect_silent(need("stats")) # Assumes stats package is always installed
})
