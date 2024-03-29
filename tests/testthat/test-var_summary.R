test_that("count_nas is correct", {
  expect_message(mt <- count_nas(mtcars), "no NAs in the data")
  expect_identical(length(mt), 0L)
  expect_true(is.integer(mt))
  expect_gt(sum(count_nas(airquality)), 0)
})

test_that("var_summary works", {
  df <- data.frame()
  expect_message(vv <- var_summary(df), "zero columns")
  expect_type(vv, "list")
  expect_error(var_summary(list(1)), "must be a data frame")
})

test_that("var_summary is correct", {
  x <- tibble::tibble(a = 1:4,
                      b = c(1, 1, NA, NaN),
                      c = c(".", "", "NA", NA))
  expect_snapshot_output(var_summary(x, na_strings = c("", "NA")))
})
