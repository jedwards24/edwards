test_that("compare_vecs() is correct", {
  #x1 <- lubridate::dmy(c("10-03-17", "20-01-16"))
  #expect_error(compare_vecs(x1, c("a", "b")))
  expect_warning(compare_vecs(1:2, c("1", "2")), "Vectors have different classes")
  expect_equal(compare_vecs(1:10, 1:10)$prop[1], 1)
  expect_error(compare_vecs(1:4, 1:3), "same length")
  expect_error(compare_vecs(list(1:2), 1:2), "must be atomic")
  x <- c(1, 2, 3)
  y <- c(1, 1.9, 3 + 1E-10)
  ct1 <- compare_vecs(x, y, tol = 0)$count
  ct2 <- compare_vecs(x, y, tol = 1E-5)$count
  expect_equal(ct1, c(1, 1, 1, 0, 0, 0))
  expect_equal(ct2, c(2, 1, 0, 0, 0, 0))
  x <- c(1, 2, NA, NA)
  y <- c(1, 1, 1, NA)
  expect_equal(compare_vecs(x, y, na.rm = FALSE)$prop[1], 0.25)
  expect_equal(compare_vecs(x, y, na.rm = TRUE)$prop[1], 0.5)
  #expect_is(compare_vecs(1:10, 1:10), "data.frame")
})

test_that("is_one2one() is correct", {
  d1 <- tibble::tibble(x = 1:5,
               y = c("a", "b", "c", "d", "e"),
               z = c(1 : 4, 2))
  expect_true(is_one2one(d1, x, y))
  expect_message(res <- is_one2one(d1, 1:3), "Column z")
  expect_false(res)
})

test_that("compare_sets() is correct", {
  x <- letters[1:5]
  y <- letters[4:6]
  tb1 <- compare_sets(x, y, TRUE)
  tb2 <- compare_sets(x, y, FALSE)
  expect_s3_class(tb1, "data.frame")
  expect_equal(nrow(tb1), 3)
  expect_equal(sort(tb2$element), sort(union(x, y)))
  expect_equal(tb1$count[1], sum(tb2$both))
  expect_equal(tb1$count[2], sum(tb2$just_x))
  expect_equal(tb1$count[3], sum(tb2$just_y))
})

test_that("find_similar() is correct", {
  dt <- tibble::tibble(a = 0:4,
                        b = c(0L, 1L, 6L, 6L, NA),
                        c = as.double(0:4),
                        u = letters[1:5],
                        v = c(NA, "b", "c", "f", NA),
                        w = c("aa", "b", NA, "f", NA))
  res_expect <- tibble::tribble(
    ~var1, ~var2,      ~class, ~match, ~match_zero, ~both_na, ~na_1, ~na_2, ~diff,    ~prop_match_nz,
    "a",   "b",   "integer",     2L,          1L,       0L,    0L,    1L,    2L,       1/3,
    "u",   "v", "character",     2L,          0L,       0L,    0L,    2L,    1L,       2/3,
    "u",   "w", "character",     1L,          0L,       0L,    0L,    2L,    2L,       1/3,
    "v",   "w", "character",     2L,          0L,       1L,    1L,    1L,    0L,       1
  )
  dt2 <- tibble::tibble(b2 = c(0L, 1L, 6L, 6L, NA),
                        c2 = as.double(0:4),
                        v2 = c(NA, "b", "c", "f", NA))

  res_expect2 <- tibble::tribble(
    ~var1, ~var2,      ~class, ~match, ~match_zero, ~both_na, ~na_1, ~na_2, ~diff,    ~prop_match_nz,
    "a",  "b2",   "integer",     2L,          1L,       0L,    0L,    1L,    2L,               1/3,
    "b",  "b2",   "integer",     4L,          1L,       1L,    0L,    0L,    0L,                 1,
    "c",  "c2",   "numeric",     5L,          1L,       0L,    0L,    0L,    0L,                 1,
    "u",  "v2", "character",     2L,          0L,       0L,    0L,    2L,    1L,               2/3,
    "v",  "v2", "character",     3L,          0L,       2L,    0L,    0L,    0L,                 1,
    "w",  "v2", "character",     2L,          0L,       1L,    1L,    1L,    0L,                 1
  )
  expect_equal(res_expect, find_similar(dt))
  expect_equal(res_expect2, find_similar(dt, dt2))
  # test zero row inputs
  expect_identical(dim(find_similar(data.frame())), c(0L, 10L))
  dt3 <- tibble::tibble(a = integer(0), b = integer(0))
  dt4 <- tibble::tibble(a = 1:3, b = letters[1:3])
  expect_identical(dim(find_similar(dt3)), c(1L, 10L))
  expect_identical(dim(find_similar(dt4)), c(0L, 10L))
})
