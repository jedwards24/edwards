test_that("test_if_any() works", {
  x <- c(3:1, 2L)
  expect_equal(na_if_any(x, 2), c(3L, NA, 1, NA))
  expect_equal(na_if_any(x, 2:3), c(NA, NA, 1, NA))
})

test_that("test_if_any() works", {
df <- data.frame(x1 = c("a", "", "1"),
                 x2 = c("-", "", "b"),
                 x3 = 1:3)
out_expect <- data.frame(x1 = c("a", NA, NA),
                        x2 = c(NA, NA, "b"),
                        x3 = 1:3)
expect_identical(na_if_string(df, c("-", "", "1")), out_expect)
})
