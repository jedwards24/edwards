context("Comparison functions")

test_that("compare_vecs() is correct", {
  #x1 <- lubridate::dmy(c("10-03-17", "20-01-16"))
  #expect_error(compare_vecs(x1, c("a", "b")))
  expect_warning(compare_vecs(1:2, c("1", "2")), "Vectors have different classes")
  expect_equal(compare_vecs(1:10, 1:10)$prop[1], 1)
  expect_error(compare_vecs(1:4, 1:3), "same length")
  expect_error(compare_vecs(list(1:2), 1:2), "must be atomic")
  #expect_is(compare_vecs(1:10, 1:10), "data.frame")
})
