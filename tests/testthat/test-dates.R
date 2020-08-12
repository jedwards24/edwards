test_that("is_simple_datetime() works", {
  x <- as.POSIXct("2009-08-03")
  y <- as.Date(x)
  expect_true(is_simple_datetime(x))
  expect_false(is_simple_datetime(x + 1))
  expect_false(is_simple_datetime(y))
})

test_that("diff_days() works", {
  d1 <- lubridate::ymd_hms("2020-02-01 08:00:00")
  d2 <- lubridate::ymd_hms("2020-01-01 00:00:00")
  d3 <- lubridate::ymd("2020-01-01")
  expect_equal(diff_days(d1, d2, keep_times = FALSE), 31)
  expect_equal(diff_days(d1, d2, keep_times = TRUE), 94/3)
  expect_equal(diff_days(d1, d3, keep_times = TRUE), 31)
  expect_error(diff_days(d1, "2020-01-01"), "`date2` must be")
})

