test_that("is_simple_datetime() works", {
  x <- as.POSIXct("2009-08-03")
  y <- as.Date(x)
  expect_true(is_simple_datetime(x))
  expect_false(is_simple_datetime(x + 1))
  expect_false(is_simple_datetime(y))
})
