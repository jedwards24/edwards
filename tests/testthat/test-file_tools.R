test_that("latest_file() works", {
  dir <- "../testdata/file_tools"
  expect_warning(lat1 <- suppressMessages(latest_file(dir, "test", verbose = TRUE)))
  expect_warning(lat2 <- suppressMessages(latest_file(dir, "test", file_ext = "rds", verbose = TRUE)))
  expect_warning(lat3 <- suppressMessages(latest_file(dir, ".*test", verbose = TRUE)))

  expect_identical(lat1, "../testdata/file_tools/test_99.RDATA")
  expect_identical(lat2, "../testdata/file_tools/test_201.RDS")
  expect_identical(lat3, "../testdata/file_tools/xtest_222.RDS")
  expect_identical(latest_file(dir, "test", n = 2, silent = TRUE), "../testdata/file_tools/test_201.RDS")
})

test_that("save_check() does not overwrite incorrectly", {
  testthat::expect_message(save_check(1, "../testdata/file_tools/test_20.RDS", version = 2), "Output not saved")
})

test_that("dir_...() functions run", {
  expect_type(dir_contents("../testdata"), "list")
  expect_type(dir_files("../testdata"), "list")
  expect_type(dir_size("../testdata"), "double")
})
