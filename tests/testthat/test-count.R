context("Counting functions")

x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), c = 1:3)
y <- tibble::tibble(a = c("a", ".", ".", "a"), b = c("-", "-", "b", "b"), c = c("n/a", "na", "f", ""))
strs <- c(".", "-", "n/a", "na", "")
y <- tibble::tibble(a = c("a", ".", ".", "a"),
                    b = c("-", "-", "b", "b"),
                    c = rep("z", 4),
                    d = c("n/a", "f", "f", ""))
strs <- c(".", "-", "n/a", "na", "")


test_that("count_string is correct", {
  expect_identical(count_string(x, "an"), c(a = 3L))
  expect_identical(count_string(x, "an", all = T), c(a = 3L, b = 0L, c = 0L))
  expect_identical(count_string(x, "nun", all = T), c(a = 0L, b = 0L, c = 0L))
  expect_identical(length(count_string(x, "un")), 0L)
  expect_message(count_string(x, "un"), "not found in the data")
  expect_identical(length(count_string(x, "1")), 0L)
})

test_that("count_matches is correct", {
  expect_identical(count_matches(x, "an"), c(a = 1L))
  expect_message(count_matches(x, "bo"), "No matches in the data")
  expect_identical(count_matches(x, 1L), c(c = 1L))
  expect_identical(length(count_matches(x, 1)), 0L)
  expect_identical(length(count_matches(x, "1")), 0L)
})

test_that("count_matches2 works", {
  tb1 <- count_matches2(y, strs, TRUE)
  tb2 <- count_matches2(y[, 1], strs, TRUE)
  tb3 <- count_matches2(y, ".", TRUE)
  tb4 <- count_matches2(y[, 1], ".", TRUE)
  n_str <- length(strs)
  ycol <- ncol(y)
  expect_identical(dim(tb1), c(ycol, n_str + 1L))
  expect_identical(dim(tb2), c(1L, n_str + 1L))
  expect_identical(dim(tb3), c(ycol, 2L))
  expect_identical(dim(tb4), c(1L, 2L))
  expect_identical(count_matches2(y, strs), count_matches2(y, c(strs, "x")))
  expect_identical(names(tb1), c("col_names", strs))
  expect_message(count_matches2(y, "x"), "No matches in the data")
})

test_that("count_nas is correct", {
  mt <- count_nas(mtcars)
  expect_message(count_nas(mtcars), "no NAs in the data")
  expect_identical(length(mt), 0L)
  expect_true(is.integer(mt))
  expect_gt(sum(count_nas(airquality)), 0)
})

test_that("var_summary works", {
  df <- data.frame()
  expect_message(var_summary(df), "zero columns")
  expect_type(var_summary(df), "list")
  expect_error(var_summary(list(1)), "must be a data frame")
})


test_that("count2 is correct", {
  tb <- tibble::tibble(x = c(1, 3, 2, 3), y = 1:4)
  tb2 <- dplyr::mutate(tb, n = 3)
  x1 <- count2(tb, x)
  x2 <- count2(tb, x, wt = y)
  x3 <- count2(tb, x, name = "nn")
  x4 <- count2(tb2, x, n)
  x5 <- dplyr::select(count2(tb, x, sort = FALSE), -prop)

  y1 <- dplyr::count(tb, x, sort = TRUE) %>% dplyr::mutate(prop = n / sum(n))
  y2 <- dplyr::count(tb, x, wt = y, sort = TRUE) %>% dplyr::mutate(prop = n / sum(n))
  y3 <- dplyr::count(tb, x, name = "nn", sort = TRUE) %>% dplyr::mutate(prop = nn / sum(nn))
  y4 <- dplyr::count(tb2, x, n, sort = TRUE) %>% dplyr::mutate(prop = nn / sum(nn))
  expect_identical(x1, y1)
  expect_identical(x2, y2)
  expect_identical(x3, y3)
  expect_identical(x4, y4)
  expect_identical(x5, dplyr::count(tb, x))
})
