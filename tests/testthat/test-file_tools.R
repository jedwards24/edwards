test_that("latest_file() works", {
  dir <- "../testdata/file_tools"
  expect_identical(latest_file(dir, "test"), "../testdata/file_tools/test_99.RDATA")
  expect_identical(latest_file(dir, "test", "RDS"), "../testdata/file_tools/test_201.RDS")
  expect_identical(latest_file(dir, ".*test"), "../testdata/file_tools/xtest_222.RDS")
})
