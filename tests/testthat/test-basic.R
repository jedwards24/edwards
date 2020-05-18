test_that("mode_stat() works", {
  x <- c(1:5, 2L, 3L)
  expect_equal(mode_stat(x, F), 2L)
  expect_equal(mode_stat(x, T), c(2L, 3L))
})
