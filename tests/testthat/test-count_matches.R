x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), c = 1:3)
y <- tibble::tibble(a = c("a", ".", ".", "a"), b = c("-", "-", "b", "b"), c = c("n/a", "na", "f", ""))
strs <- c(".", "-", "n/a", "na", "")
y <- tibble::tibble(a = c("a", ".", ".", "a"),
                    b = c("-", "-", "b", "b"),
                    c = rep("z", 4),
                    d = c("n/a", "f", "f", ""))
strs <- c(".", "-", "n/a", "na", "")

test_that("count_pattern is correct", {
  expect_identical(count_pattern(x, "an"), c(a = 3L))
  expect_identical(count_pattern(x, "an", all = T), c(a = 3L, b = 0L, c = 0L))
  expect_identical(count_pattern(x, "nun", all = T), c(a = 0L, b = 0L, c = 0L))
  expect_message(res1 <- count_pattern(x, "un"), "not found in the data")
  expect_identical(res1, rlang::set_names(integer(0L)))
  expect_message(res2 <- count_pattern(x, "1"), "not found in the data")
  expect_identical(res2, rlang::set_names(integer(0L)))
})

test_that("count_matches is correct", {
  expect_identical(count_matches(x, "an"), c(a = 1L))
  expect_message(count_matches(x, "bo"), "No matches in the data")
  expect_identical(count_matches(x, 1L), c(c = 1L))
  suppressMessages({
    expect_identical(length(count_matches(x, 1)), 0L)
    expect_identical(length(count_matches(x, "1")), 0L)
  })
  expect_true(all(dplyr::between(count_matches(x, "an", all = TRUE, prop = TRUE), 0, 1)))
})

test_that("count_matches with length(values) > 1 works", {
  tb1 <- count_matches(y, strs, TRUE, detail=TRUE)
  tb2 <- count_matches(y[, 1], strs, TRUE, detail=TRUE)
  tb3 <- count_matches(y, ".", TRUE, detail=TRUE)
  tb4 <- count_matches(y[, 1], ".", TRUE, detail=TRUE)
  expect_identical(count_matches(y, strs, TRUE, detail=FALSE), c(a = 2L, b = 2L, c = 0L, d = 2L))
  n_str <- length(strs)
  ycol <- ncol(y)
  expect_identical(dim(tb1), c(n_str, ycol + 1L))
  expect_identical(dim(tb2), c(n_str, 2L))
  expect_identical(dim(tb3), c(1L, ycol + 1L))
  expect_identical(dim(tb4), c(1L, 2L))
  expect_identical(count_matches(y, strs, detail=TRUE), count_matches(y, c(strs, "x"), detail=TRUE))
  expect_identical(names(tb1), c("value", colnames(y)))
  expect_identical(tb1$value, strs)
  expect_message(count_matches(y, "x", detail=TRUE), "No matches in the data")
})
