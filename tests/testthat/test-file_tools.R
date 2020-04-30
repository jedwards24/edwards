test_that("latest_file() works", {
  dir <- "../testdata/file_tools"
  expect_warning(lat1 <- latest_file(dir, "test"))
  expect_warning(lat2 <- latest_file(dir, "test", "RDS"))
  expect_warning(lat3 <- latest_file(dir, ".*test"))

  expect_identical(lat1, "../testdata/file_tools/test_99.RDATA", "xtest_222.RDS")
  expect_identical(lat2, "../testdata/file_tools/test_201.RDS", "test_99.RDATA")
  expect_identical(lat3, "../testdata/file_tools/xtest_222.RDS", "test333.RDS")
})