test_that("fd_cols() works", {
  df_test <- tibble::tibble(A = rep(letters[1:2], each = 3),
                 B = c(1:3, 1L, 1L, 1L),
                 C = c(1:3, 4L, 4L, 4L),
                 D = 1:6)
  expect_identical(fd_cols(df_test, D), c("D", "A", "B", "C"))
  expect_identical(fd_cols(df_test, A, B), c("A", "B", "C"))
  expect_identical(fd_cols(df_test, A), c("A"))
  expect_identical(fd_cols(df_test, c("A", "B")), c("A", "B", "C"))
  expect_identical(fd_cols(df_test, 1:2), c("A", "B", "C"))
  df_test[6, 3] <- NA_integer_
  expect_warning(val <- fd_cols(df_test, A, B), "contains missing values")
  expect_identical(val, c("A", "B"))
})
